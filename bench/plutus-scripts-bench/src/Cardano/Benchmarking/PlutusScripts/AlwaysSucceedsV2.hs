{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- PlutusV2 must be compiled using plc 1.0
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Cardano.Benchmarking.PlutusScripts.AlwaysSucceedsV2 (script) where

import           Cardano.Api (PlutusScript, PlutusScriptV2, Script (..), toScriptInAnyLang)
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptVersion (..))

import           Cardano.Benchmarking.ScriptAPI
import qualified PlutusLedgerApi.V2 as PlutusV2

import           Prelude as Haskell (String, (.), (<$>))

import qualified Data.ByteString.Short as SBS

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified PlutusTx
import           PlutusTx.Prelude as P hiding (Semigroup (..), (.), (<$>))


scriptName :: Haskell.String
scriptName
  = prepareScriptName $(LitE . StringL . loc_module <$> qLocation)

script :: PlutusBenchScript
script = mkPlutusBenchScript scriptName (toScriptInAnyLang (PlutusScript PlutusScriptV2 scriptSerialized))


{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _datum _ _txContext = ()


alwaysSucceedsShortBs :: SBS.ShortByteString
alwaysSucceedsShortBs = PlutusV2.serialiseCompiledCode $$(PlutusTx.compile [|| mkValidator ||])

scriptSerialized :: PlutusScript PlutusScriptV2
scriptSerialized = PlutusScriptSerialised alwaysSucceedsShortBs
