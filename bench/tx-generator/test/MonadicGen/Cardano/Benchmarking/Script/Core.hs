{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use uncurry" -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MonadicGen.Cardano.Benchmarking.Script.Core
where

import           Control.Arrow ((|||))
import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Control.Monad.Trans.RWS ()
import           Control.Monad.Writer (tell)
import           "contra-tracer" Control.Tracer (nullTracer)
import           Data.ByteString.Lazy.Char8 as BSL (writeFile)
import           Data.Ratio ((%))

import           Streaming
import qualified Streaming.Prelude as Streaming

import qualified Data.Text as Text (unpack)
import           Prelude

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScriptOrReferenceInput (..), ProtocolParameters,
                   ShelleyLedgerEra, convertToLedgerProtocolParameters, protocolParamMaxTxExUnits,
                   protocolParamPrices)

import qualified Cardano.Ledger.Core as Ledger

import           MonadicGen.Cardano.TxGenerator.Fund as Fund
import qualified MonadicGen.Cardano.TxGenerator.FundQueue as FundQueue
import           MonadicGen.Cardano.TxGenerator.Setup.Plutus as Plutus
import           MonadicGen.Cardano.TxGenerator.Tx
import           MonadicGen.Cardano.TxGenerator.Types
import qualified MonadicGen.Cardano.TxGenerator.Utils as Utils
import           MonadicGen.Cardano.TxGenerator.UTxO

import           MonadicGen.Cardano.Benchmarking.GeneratorTx as GeneratorTx (AsyncBenchmarkControl)
import qualified MonadicGen.Cardano.Benchmarking.GeneratorTx as GeneratorTx (waitBenchmark,
                   walletBenchmark)
import           MonadicGen.Cardano.Benchmarking.GeneratorTx.NodeToNode (ConnectClient,
                   benchmarkConnectTxSubmit)
import           MonadicGen.Cardano.Benchmarking.GeneratorTx.SizedMetadata (mkMetadata)
import qualified MonadicGen.Cardano.TxGenerator.Genesis as Genesis
import           MonadicGen.Cardano.TxGenerator.PlutusContext
import           MonadicGen.Cardano.TxGenerator.Setup.SigningKey

import           MonadicGen.Cardano.Benchmarking.OuroborosImports as Core (LocalSubmitTx,
                   SigningKeyFile, makeLocalConnectInfo, protocolToCodecConfig)

import           MonadicGen.Cardano.Benchmarking.LogTypes as Core (TraceBenchTxSubmit (..),
                   btConnect_, btN2N_, btSubmission2_, btTxSubmit_)
import           MonadicGen.Cardano.Benchmarking.Types as Core (SubmissionErrorPolicy (..))
import           MonadicGen.Cardano.Benchmarking.Wallet as Wallet

import           MonadicGen.Cardano.Benchmarking.Script.Aeson (prettyPrintOrdered,
                   readProtocolParametersFile)
import           MonadicGen.Cardano.Benchmarking.Script.Env hiding (Error (TxGenError))
import qualified MonadicGen.Cardano.Benchmarking.Script.Env as Env (Error (TxGenError))
import           MonadicGen.Cardano.Benchmarking.Script.Types
import           MonadicGen.Cardano.Benchmarking.Version as Version

data TxListElem = forall era. IsShelleyBasedEra era => TxListElem (Tx era)

instance Show TxListElem where
  show (TxListElem tx) = show tx

liftCoreWithEra :: AnyCardanoEra -> (forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO x) -> ActionM' [TxListElem] (Either TxGenError x)
liftCoreWithEra era coreCall = withEra era ( liftIO . runExceptT . coreCall)

withEra :: AnyCardanoEra -> (forall era. IsShelleyBasedEra era => AsType era -> ActionM' [TxListElem] x) -> ActionM' [TxListElem] x
withEra era action = do
  case era of
    AnyCardanoEra ConwayEra  -> action AsConwayEra
    AnyCardanoEra BabbageEra -> action AsBabbageEra
    AnyCardanoEra AlonzoEra  -> action AsAlonzoEra
    AnyCardanoEra MaryEra    -> action AsMaryEra
    AnyCardanoEra AllegraEra -> action AsAllegraEra
    AnyCardanoEra ShelleyEra -> action AsShelleyEra
    AnyCardanoEra ByronEra   -> error "byron not supported"

setProtocolParameters :: Monoid w => ProtocolParametersSource -> ActionM' w ()
setProtocolParameters s = case s of
  QueryLocalNode -> do
    setProtoParamMode ProtocolParameterQuery
  UseLocalProtocolFile file -> do
    protocolParameters <- liftIO $ readProtocolParametersFile file
    setProtoParamMode $ ProtocolParameterLocal protocolParameters

readSigningKey :: String -> SigningKeyFile In -> ActionM' [TxListElem] ()
readSigningKey name filePath =
  liftIO (readSigningKeyFile filePath) >>= \case
    Left err -> liftTxGenError err
    Right key -> setEnvKeys name key

defineSigningKey :: String -> SigningKey PaymentKey -> ActionM' [TxListElem] ()
defineSigningKey = setEnvKeys

addFund :: AnyCardanoEra -> String -> TxIn -> Lovelace -> String -> ActionM' [TxListElem] ()
addFund era wallet txIn lovelace keyName = do
  fundKey  <- getEnvKeys keyName
  let
    mkOutValue :: forall era . IsShelleyBasedEra era => AsType era -> ActionM' [TxListElem] (InAnyCardanoEra TxOutValue)
    mkOutValue _ = return $ InAnyCardanoEra (cardanoEra @era) (lovelaceToTxOutValue lovelace)
  outValue <- withEra era mkOutValue
  addFundToWallet wallet txIn outValue fundKey

addFundToWallet :: String -> TxIn -> InAnyCardanoEra TxOutValue -> SigningKey PaymentKey -> ActionM' [TxListElem] ()
addFundToWallet wallet txIn outVal skey = do
  walletRef <- getEnvWallets wallet
  liftIO (walletRefInsertFund walletRef (FundQueue.Fund $ mkFund outVal))
  where
    mkFund = Utils.liftAnyEra $ \value -> FundInEra {
           _fundTxIn = txIn
         , _fundWitness = KeyWitness KeyWitnessForSpending
         , _fundVal = value
         , _fundSigningKey = Just skey
         }

getLocalSubmitTx :: ActionM' [TxListElem] LocalSubmitTx
getLocalSubmitTx = submitTxToNodeLocal <$> getLocalConnectInfo

delay :: Double -> ActionM' [TxListElem] ()
delay t = liftIO $ threadDelay $ floor $ 1_000_000 * t

waitBenchmarkCore :: AsyncBenchmarkControl -> ActionM' [TxListElem] ()
waitBenchmarkCore ctl = do
  tracers  <- getBenchTracers
  _ <- liftIO $ runExceptT $ GeneratorTx.waitBenchmark (btTxSubmit_ tracers) ctl
  return ()

getConnectClient :: Monoid w => ActionM' w ConnectClient
getConnectClient = do
  tracers  <- getBenchTracers
  (Testnet networkMagic) <- getEnvNetworkId
  protocol <- getEnvProtocol
  void $ return $ btSubmission2_ tracers
  ioManager <- askIOManager
  return $ benchmarkConnectTxSubmit
                       ioManager
                       (btConnect_ tracers)
                       nullTracer -- (btSubmission2_ tracers)
                       (protocolToCodecConfig protocol)
                       networkMagic
waitBenchmark :: String -> ActionM' [TxListElem] ()
waitBenchmark n = getEnvThreads n >>= waitBenchmarkCore

cancelBenchmark :: String -> ActionM' [TxListElem] ()
cancelBenchmark n = do
  ctl@(_, _ , _ , shutdownAction) <- getEnvThreads n
  liftIO shutdownAction
  waitBenchmarkCore ctl

getLocalConnectInfo :: Monoid w => ActionM' w (LocalNodeConnectInfo CardanoMode)
getLocalConnectInfo = makeLocalConnectInfo <$> getEnvNetworkId <*> getEnvSocketPath

queryEra :: Monoid w => ActionM' w AnyCardanoEra
queryEra = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  ret <- liftIO $ queryNodeLocalState localNodeConnectInfo (Just $ chainTipToChainPoint chainTip) $ QueryCurrentEra CardanoModeIsMultiEra
  case ret of
    Right era -> return era
    Left err -> liftTxGenError $ TxGenError $ show err

queryRemoteProtocolParameters :: forall w. Monoid w => ActionM' w ProtocolParameters
queryRemoteProtocolParameters = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  era <- queryEra
  let
    callQuery :: forall era.
                 EraInMode era CardanoMode
              -> QueryInEra era (Ledger.PParams (ShelleyLedgerEra era))
              -> ActionM' w ProtocolParameters
    callQuery eraInMode query@(QueryInShelleyBasedEra shelleyEra _) = do
      res <- liftIO $ queryNodeLocalState localNodeConnectInfo (Just $ chainTipToChainPoint chainTip) (QueryInEra eraInMode query)
      case res of
        Right (Right pp) -> do
          let pp' = fromLedgerPParams shelleyEra pp
              pparamsFile = "protocol-parameters-queried.json"
          liftIO $ BSL.writeFile pparamsFile $ prettyPrintOrdered pp'
          traceDebug $ "queryRemoteProtocolParameters : query result saved in: " ++ pparamsFile
          return pp'
        Right (Left err) -> liftTxGenError $ TxGenError $ show err
        Left err -> liftTxGenError $ TxGenError $ show err
  case era of
    AnyCardanoEra ByronEra   -> liftTxGenError $ TxGenError "queryRemoteProtocolParameters Byron not supported"
    AnyCardanoEra ShelleyEra -> callQuery ShelleyEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraShelley QueryProtocolParameters
    AnyCardanoEra AllegraEra -> callQuery AllegraEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraAllegra QueryProtocolParameters
    AnyCardanoEra MaryEra    -> callQuery MaryEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraMary    QueryProtocolParameters
    AnyCardanoEra AlonzoEra  -> callQuery AlonzoEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    AnyCardanoEra BabbageEra -> callQuery BabbageEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraBabbage QueryProtocolParameters
    AnyCardanoEra ConwayEra  -> callQuery ConwayEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraConway QueryProtocolParameters

getProtocolParameters :: Monoid w => ActionM' w ProtocolParameters
getProtocolParameters = do
  getProtoParamMode  >>= \case
    ProtocolParameterQuery -> queryRemoteProtocolParameters
    ProtocolParameterLocal parameters -> return parameters

waitForEra :: AnyCardanoEra -> ActionM' [TxListElem] ()
waitForEra era = do
  currentEra <- queryEra
  if currentEra == era
    then return ()
    else do
      traceError $ "Current era: " ++ show currentEra ++ " Waiting for: " ++ show era
      liftIO $ threadDelay 1_000_000
      waitForEra era

localSubmitTx :: TxInMode CardanoMode -> ActionM' [TxListElem] (SubmitResult (TxValidationErrorInMode CardanoMode))
localSubmitTx tx = do
  submit <- getLocalSubmitTx
  ret <- liftIO $ submit tx
  case ret of
    SubmitSuccess -> return ret
    SubmitFail e -> do
      let msg = concat [ "local submit failed: " , show e , " (" , show tx , ")" ]
      traceDebug msg
      return ret
--      throwE $ ApiError msg

-- TODO:
-- It should be possible to exit the tx-generator with an exception and also get the log messages.
-- Problem 1: When doing throwE $ ApiError msg logmessages get lost !
-- Problem 2: Workbench restarts the tx-generator -> this may be the reason for loss of messages

toMetadata :: forall era. IsShelleyBasedEra era => Maybe Int -> TxMetadataInEra era
toMetadata Nothing = TxMetadataNone
toMetadata (Just payloadSize) = case mkMetadata payloadSize of
  Right m -> m
  Left err -> error err

submitAction :: AnyCardanoEra -> SubmitMode -> Generator -> TxGenTxParams -> ActionM' [TxListElem] ()
submitAction era submitMode generator txParams = withEra era $ submitInEra submitMode generator txParams

submitInEra :: forall era. IsShelleyBasedEra era => SubmitMode -> Generator -> TxGenTxParams -> AsType era -> ActionM' [TxListElem] ()
submitInEra submitMode generator txParams era = do
  txStream <- evalGenerator generator txParams era
  case submitMode of
    NodeToNode _ -> error "NodeToNode deprecated: ToDo: remove"
    Benchmark nodes threadName tpsRate txCount -> benchmarkTxStream txStream nodes threadName tpsRate txCount era
    LocalSocket -> submitAll (void . localSubmitTx . Utils.mkTxInModeCardano) txStream
    DumpToFile filePath -> liftIO $ Streaming.writeFile filePath $ Streaming.map showTx txStream
    DiscardTX -> liftIO $ Streaming.mapM_ forceTx txStream
 where
  forceTx (Right _) = return ()
  forceTx (Left err) = error $ show err
  showTx (Left err) = error $ show err
  showTx (Right tx) = '\n' : show tx
   -- todo: use Streaming.run
  submitAll :: (Tx era -> ActionM' [TxListElem] ()) -> TxStream IO era -> ActionM' [TxListElem] ()
  submitAll callback stream = do
    step <- liftIO $ Streaming.inspect stream
    case step of
      (Left ()) -> return ()
      (Right (Left err :> _rest)) -> liftTxGenError $ TxGenError $ show err
      (Right (Right tx :> rest)) -> do
        callback tx
        submitAll callback rest

benchmarkTxStream :: forall w era. (Monoid w, IsShelleyBasedEra era)
  => TxStream IO era
  -> TargetNodes
  -> String
  -> TPSRate
  -> NumberOfTxs
  -> AsType era
  -> ActionM' w ()
benchmarkTxStream txStream targetNodes threadName tps txCount era = do
  tracers  <- getBenchTracers
  connectClient <- getConnectClient
  let
    coreCall :: AsType era -> ExceptT TxGenError IO AsyncBenchmarkControl
    coreCall eraProxy = GeneratorTx.walletBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient
                                               threadName targetNodes tps LogErrors eraProxy txCount txStream
  ret <- liftIO $ runExceptT $ coreCall era
  case ret of
    Left err -> liftTxGenError err
    Right ctl -> setEnvThreads threadName ctl

evalGenerator :: forall era. IsShelleyBasedEra era => Generator -> TxGenTxParams -> AsType era -> ActionM' [TxListElem] (TxStream IO era)
evalGenerator generator txParams@TxGenTxParams{txParamFee = fee} era = do
  networkId <- getEnvNetworkId
  protocolParameters <- getProtocolParameters

  case convertToLedgerProtocolParameters (shelleyBasedEra @era) protocolParameters of
    Left err -> throwE (Env.TxGenError (ApiError err))
    Right ledgerParameters ->
      case generator of
        SecureGenesis wallet genesisKeyName destKeyName -> do
          genesis  <- getEnvGenesis
          destKey  <- getEnvKeys destKeyName
          destWallet  <- getEnvWallets wallet
          genesisKey  <- getEnvKeys genesisKeyName
          (tx, fund) <- firstExceptT Env.TxGenError $ hoistEither $
            Genesis.genesisSecureInitialFund networkId genesis genesisKey destKey txParams
          let
            gen = do
              walletRefInsertFund destWallet fund
              return $ Right tx
          tell [TxListElem tx]
          return $ Streaming.effect (Streaming.yield <$> gen)

        -- 'Split' combines regular payments and payments for change.
        -- There are lists of payments buried in the 'PayWithChange'
        -- type conditionally sent back by 'Utils.includeChange', to
        -- then be used while partially applied as the @valueSplitter@
        -- in 'sourceToStoreTransactionNew'.
        Split walletName payMode payModeChange coins -> do
          wallet <- getEnvWallets walletName
          (toUTxO, addressOut) <- interpretPayMode payMode
          traceDebug $ "split output address : " ++ addressOut
          (toUTxOChange, addressChange) <- interpretPayMode payModeChange
          traceDebug $ "split change address : " ++ addressChange
          let
            inToOut = return . Utils.includeChange fee coins
            txGenerator = genTx (cardanoEra @era) ledgerParameters (TxInsCollateralNone, []) feeInEra TxMetadataNone
          inputFunds <- liftToAction $ walletSource wallet 1
          sourceToStore <- withTxGenError . sourceToStoreTransactionNew txGenerator inputFunds inToOut $ mangleWithChange (liftIOCreateAndStore toUTxOChange) (liftIOCreateAndStore toUTxO)
          tell [TxListElem sourceToStore]
          return . Streaming.effect . pure . Streaming.yield $ Right sourceToStore

        -- The 'SplitN' case's call chain is somewhat elaborate.
        -- The division is done in 'Utils.inputsToOutputsWithFee'
        -- but things are threaded through
        -- 'MonadicGen.Cardano.Benchmarking.Wallet.mangle' and packed into
        -- the transaction assembled by 'sourceToStoreTransactionNew'.
        SplitN walletName payMode count -> do
          wallet <- getEnvWallets walletName
          (toUTxO, addressOut) <- interpretPayMode payMode
          traceDebug $ "SplitN output address : " ++ addressOut
          let
            inToOut = withExceptT TxGenError . Utils.inputsToOutputsWithFee fee count
            txGenerator = genTx (cardanoEra @era) ledgerParameters (TxInsCollateralNone, []) feeInEra TxMetadataNone
          inputFunds <- liftToAction $ walletSource wallet 1
          sourceToStore <- withTxGenError $ sourceToStoreTransactionNew txGenerator inputFunds inToOut (mangle . repeat $ liftIOCreateAndStore toUTxO)
          tell [TxListElem sourceToStore]
          return . Streaming.effect . pure . Streaming.yield $ Right sourceToStore

        NtoM walletName payMode inputs outputs metadataSize collateralWallet -> do
          wallet <- getEnvWallets walletName
          collaterals <- selectCollateralFunds collateralWallet
          (toUTxO, addressOut) <- interpretPayMode payMode
          traceDebug $ "NtoM output address : " ++ addressOut
          let
            inToOut = withExceptT TxGenError . Utils.inputsToOutputsWithFee fee outputs
            txGenerator = genTx (cardanoEra @era) ledgerParameters collaterals feeInEra (toMetadata metadataSize)
            previewCatcher err = do
              traceDebug $ "Error creating Tx preview: " ++ show err
              throwE err
          inputFunds <- liftToAction $ walletSource wallet inputs
          sourceToStore <- withTxGenError $ sourceToStoreTransactionNew txGenerator inputFunds inToOut (mangle . repeat $ liftIOCreateAndStore toUTxO)
          tell [TxListElem sourceToStore]

          fundPreview <- liftIO $ walletPreview wallet inputs
          preview <- withTxGenError (sourceTransactionPreview txGenerator fundPreview inToOut (mangle . repeat $ liftIOCreateAndStore toUTxO))
                       `catchE` previewCatcher
          let txSize = txSizeInBytes preview
          traceDebug $ "Projected Tx size in bytes: " ++ show txSize
          summary_ <- getEnvSummary
          forM_ summary_ $ \summary -> do
            let summary' = summary {projectedTxSize = Just txSize}
            setEnvSummary summary'
            traceBenchTxSubmit TraceBenchPlutusBudgetSummary summary'
          dumpBudgetSummaryIfExisting

          return . Streaming.effect . pure . Streaming.yield $ Right sourceToStore

        Sequence l -> do
          gList <- forM l $ \g -> evalGenerator g txParams era
          return $ Streaming.for (Streaming.each gList) id

        Cycle g -> Streaming.cycle <$> evalGenerator g txParams era

        Take count g -> Streaming.take count <$> evalGenerator g txParams era

        RoundRobin l -> do
          _gList <- forM l $ \g -> evalGenerator g txParams era
          error "return $ foldr1 Streaming.interleaves gList"

        OneOf _l -> error "todo: implement Quickcheck style oneOf generator"

  where
    feeInEra = Utils.mkTxFee fee
    -- 'liftIOCreateAndStore' is supposed to be some indication that 'liftIO'
    -- is applied to a 'CreateAndStore'.
    -- This could be golfed as @((liftIO .) .)@ but it's unreadable.
    liftIOCreateAndStore cas = second (\f x y -> liftIO (f x y)) . cas

selectCollateralFunds :: forall era. IsShelleyBasedEra era
  => Maybe String
  -> ActionM' [TxListElem] (TxInsCollateral era, [FundQueue.Fund])
selectCollateralFunds Nothing = return (TxInsCollateralNone, [])
selectCollateralFunds (Just walletName) = do
  cw <- getEnvWallets walletName
  collateralFunds <- liftIO ( askWalletRef cw FundQueue.toList ) >>= \case
    [] -> throwE $ WalletError "selectCollateralFunds: emptylist"
    l -> return l
  case forEraMaybeEon (cardanoEra @era) of
      Nothing -> throwE $ WalletError $ "selectCollateralFunds: collateral: era not supported :" ++ show (cardanoEra @era)
      Just p -> return (TxInsCollateral p $  map getFundTxIn collateralFunds, collateralFunds)

dumpToFile :: Monoid w => FilePath -> TxInMode CardanoMode -> ActionM' w ()
dumpToFile filePath tx = liftIO $ dumpToFileIO filePath tx

dumpToFileIO :: FilePath -> TxInMode CardanoMode -> IO ()
dumpToFileIO filePath tx = appendFile filePath ('\n' : show tx)

initWallet :: Monoid w => String -> ActionM' w ()
initWallet name = liftIO Wallet.initWallet >>= setEnvWallets name

-- The inner monad being 'IO' creates some programming overhead above.
-- Something like 'MonadIO' would be helpful, but the typing is tricky.
interpretPayMode :: forall w era. (Monoid w, IsShelleyBasedEra era) => PayMode -> ActionM' w (CreateAndStore IO era, String)
interpretPayMode payMode = do
  networkId <- getEnvNetworkId
  case payMode of
    PayToAddr keyName destWallet -> do
      fundKey <- getEnvKeys keyName
      walletRef <- getEnvWallets destWallet
      return ( createAndStore (mkUTxOVariant networkId fundKey) (mkWalletFundStore walletRef)
             , Text.unpack $ serialiseAddress $ Utils.keyAddress @era networkId fundKey)
    PayToScript scriptSpec destWallet -> do
      walletRef <- getEnvWallets destWallet
      (witness, script, scriptData, _scriptFee) <- makePlutusContext scriptSpec
      case script of
        ScriptInAnyLang _ script' ->
          return ( createAndStore (mkUTxOScript networkId (script, scriptData) witness) (mkWalletFundStore walletRef)
                 , Text.unpack $ serialiseAddress $ makeShelleyAddress networkId (PaymentCredentialByScript $ hashScript script') NoStakeAddress )

makePlutusContext :: forall w era. (Monoid w, IsShelleyBasedEra era)
  => ScriptSpec
  -> ActionM' w (Witness WitCtxTxIn era, ScriptInAnyLang, ScriptData, Lovelace)
makePlutusContext ScriptSpec{..} = do
  protocolParameters <- getProtocolParameters
  script <- liftIOSafe $ Plutus.readPlutusScript scriptSpecFile

  executionUnitPrices <- case protocolParamPrices protocolParameters of
    Just x -> return x
    Nothing -> throwE $ WalletError "unexpected protocolParamPrices == Nothing in runPlutusBenchmark"

  perTxBudget <- case protocolParamMaxTxExUnits protocolParameters of
    Nothing -> liftTxGenError $ TxGenError "Cannot determine protocolParamMaxTxExUnits"
    Just b -> return b
  traceDebug $ "Plutus auto mode : Available budget per TX: " ++ show perTxBudget

  (scriptData, scriptRedeemer, executionUnits) <- case scriptSpecBudget of
    StaticScriptBudget sDataFile redeemerFile units withCheck -> do
      sData <- liftIOSafe (readScriptData sDataFile)
      redeemer <- liftIOSafe (readScriptData redeemerFile)
      when withCheck $ do
        unitsPreRun <- preExecuteScriptAction protocolParameters script (getScriptData sData) (getScriptData redeemer)
        unless (units == unitsPreRun) $
          throwE $ WalletError $ concat [
              " Stated execution Units do not match result of pre execution. "
            , " Stated value : ", show units
            , " PreExecution result : ", show unitsPreRun
            ]
      return (sData, redeemer, units)

    AutoScript redeemerFile txInputs -> do
      redeemer <- liftIOSafe $ readScriptData redeemerFile
      let
        strategy = case scriptSpecPlutusType of
          LimitTxPerBlock_8 -> TargetTxsPerBlock 8
          _                 -> TargetTxExpenditure

        -- reflects properties hard-coded into the loop scripts for benchmarking:
        -- 1. script datum is not used
        -- 2. the loop terminates at 1_000_000 when counting down
        -- 3. the loop's initial value is the first numerical value in the redeemer argument structure
        autoBudget = PlutusAutoBudget
          { autoBudgetUnits = perTxBudget
          , autoBudgetDatum = ScriptDataNumber 0
          , autoBudgetRedeemer = unsafeHashableScriptData $ scriptDataModifyNumber (const 1_000_000) (getScriptData redeemer)
          }
      traceDebug $ "Plutus auto mode : Available budget per Tx: " ++ show perTxBudget
                   ++ " -- split between inputs per Tx: " ++ show txInputs

      case plutusAutoScaleBlockfit protocolParameters (either ("builtin: "++) ("plutus file: "++) scriptSpecFile) script autoBudget strategy txInputs of
        Left err -> liftTxGenError err
        Right (summary, PlutusAutoBudget{..}, preRun) -> do
          setEnvSummary summary
          dumpBudgetSummaryIfExisting
          return (unsafeHashableScriptData autoBudgetDatum, autoBudgetRedeemer, preRun)

  let msg = mconcat [ "Plutus Benchmark :"
                    , " Script: ", show scriptSpecFile
                    , ", Datum: ", show scriptData
                    , ", Redeemer: ", show scriptRedeemer
                    , ", StatedBudget: ", show executionUnits
                    ]
  traceDebug msg

  let
    -- TODO --    Cardano.Ledger.Alonzo.Scripts.txscriptfee :: Prices -> ExUnits -> Coin
    scriptFee = quantityToLovelace $ Quantity $ ceiling f
       where
         f :: Rational
         f = (executionSteps e `times` priceExecutionSteps p) + (executionMemory e `times` priceExecutionMemory p)
         e = executionUnits
         p = executionUnitPrices
         times w c = fromIntegral w % 1 * c

  case script of
    ScriptInAnyLang lang (PlutusScript version script') ->
      let
        scriptWitness :: ScriptWitness WitCtxTxIn era
        scriptWitness = case scriptLanguageSupportedInEra (cardanoEra @era) lang of
          Nothing -> error $ "runPlutusBenchmark: " ++ show version ++ " not supported in era: " ++ show (cardanoEra @era)
          Just scriptLang -> PlutusScriptWitness
                              scriptLang
                              version
                              (PScript script')               -- TODO: add capability for reference inputs from Babbage era onwards
                              (ScriptDatumForTxIn scriptData)
                              scriptRedeemer
                              executionUnits
      in return (ScriptWitness ScriptWitnessForSpending scriptWitness, script, getScriptData scriptData, scriptFee)
    _ ->
      liftTxGenError $ TxGenError "runPlutusBenchmark: only Plutus scripts supported"

preExecuteScriptAction ::
     Monoid w
  => ProtocolParameters
  -> ScriptInAnyLang
  -> ScriptData
  -> ScriptData
  -> ActionM' w ExecutionUnits
preExecuteScriptAction protocolParameters script scriptData redeemer
  = case Plutus.preExecutePlutusScript protocolParameters script scriptData (unsafeHashableScriptData redeemer) of
      Left err -> throwE $ WalletError ( "makePlutusContext preExecuteScript failed: " ++ show err )
      Right costs -> return costs

dumpBudgetSummaryIfExisting :: Monoid w => ActionM' w ()
dumpBudgetSummaryIfExisting
  = do
    summary_ <- getEnvSummary
    forM_ summary_ $ \summary -> do
      liftIO $ BSL.writeFile summaryFile $ prettyPrintOrdered summary
      traceDebug $ "dumpBudgetSummaryIfExisting : budget summary created/updated in: " ++ summaryFile
  where
    summaryFile = "plutus-budget-summary.json"

traceTxGeneratorVersion :: Monoid w => ActionM' w ()
traceTxGeneratorVersion = traceBenchTxSubmit TraceTxGeneratorVersion Version.txGeneratorVersion

{-
This is for dirty hacking and testing and quick-fixes.
Its a function that can be called from the JSON scripts
and for which the JSON encoding is "reserved".
-}
reserved :: Monoid w => [String] -> ActionM' w ()
reserved _ = do
  throwE $ UserError "no dirty hack is implemented"
