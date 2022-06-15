{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Monoid law, left identity" -}

module Cardano.CLI.Shelley.Run.Genesis.Types
  ( OutputShelleyGenesis(..)
  , OutputShelleyGenesisStaking(..)
  , ListMap(..)
  , toOutputTemplate
  ) where

import Cardano.CLI.Shelley.Run.Genesis.ListMap (ListMap(..))
import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.BaseTypes (PositiveUnitInterval, Network)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Era (Era(Crypto))
import Cardano.Ledger.Keys (KeyHash, KeyRole(Genesis), GenDelegPair)
import Cardano.Ledger.Shelley.PParams ( PParams )
import Cardano.Slotting.Slot (EpochSize(..))
import Data.Aeson (ToJSON(..), (.=))
import Data.Eq (Eq)
import Data.Function (($))
import Data.Monoid (Monoid(..), mconcat)
import Data.Time (NominalDiffTime, UTCTime(..))
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Text.Show (Show)

import qualified Cardano.Ledger.Crypto as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Ledger
import qualified Cardano.Ledger.Shelley.TxBody as Ledger
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map

data OutputShelleyGenesis era = OutputShelleyGenesis
  { sgSystemStart :: !UTCTime
  , sgNetworkMagic :: !Word32
  , sgNetworkId :: !Network
  , sgActiveSlotsCoeff :: !PositiveUnitInterval
  , sgSecurityParam :: !Word64
  , sgEpochLength :: !EpochSize
  , sgSlotsPerKESPeriod :: !Word64
  , sgMaxKESEvolutions :: !Word64
  , sgSlotLength :: !NominalDiffTime
  , sgUpdateQuorum :: !Word64
  , sgMaxLovelaceSupply :: !Word64
  , sgProtocolParams :: !(PParams era)
  , sgGenDelegs :: !(ListMap (KeyHash 'Genesis (Crypto era)) (GenDelegPair (Crypto era)))
  , sgInitialFunds :: !(ListMap (Addr (Crypto era)) Coin)
  , sgStaking :: !(OutputShelleyGenesisStaking era)
  }
  deriving stock (Eq, Show, Generic)

toOutputTemplate :: Ledger.ShelleyGenesis era -> OutputShelleyGenesis era
toOutputTemplate template = OutputShelleyGenesis
  { sgSystemStart = Ledger.sgSystemStart template
  , sgNetworkMagic = Ledger.sgNetworkMagic template
  , sgNetworkId = Ledger.sgNetworkId template
  , sgActiveSlotsCoeff = Ledger.sgActiveSlotsCoeff template
  , sgSecurityParam = Ledger.sgSecurityParam template
  , sgEpochLength = Ledger.sgEpochLength template
  , sgSlotsPerKESPeriod = Ledger.sgSlotsPerKESPeriod template
  , sgMaxKESEvolutions = Ledger.sgMaxKESEvolutions template
  , sgSlotLength = Ledger.sgSlotLength template
  , sgUpdateQuorum = Ledger.sgUpdateQuorum template
  , sgMaxLovelaceSupply = Ledger.sgMaxLovelaceSupply template
  , sgProtocolParams = Ledger.sgProtocolParams template
  , sgGenDelegs = ListMap (Map.toList (Ledger.sgGenDelegs template))
  , sgInitialFunds = ListMap (Map.toList (Ledger.sgInitialFunds template))
  , sgStaking = OutputShelleyGenesisStaking
    { osgsPools = ListMap $ Map.toList $ Ledger.sgsPools $ Ledger.sgStaking template
    , osgsStake = ListMap $ Map.toList $ Ledger.sgsStake $ Ledger.sgStaking template
    }
  }

instance
  ( Ledger.Crypto (Crypto era)
  ) => ToJSON (OutputShelleyGenesis era) where
  toJSON sg =
    -- Forced evaluation of each field allows the parent object to no longer be
    -- referenced, which helps prevent retention of any fields that have already
    -- been serialised.
    let !systemStart        = sgSystemStart sg
        !networkMagic       = sgNetworkMagic sg
        !networkId          = sgNetworkId sg
        !activeSlotsCoeff   = sgActiveSlotsCoeff sg
        !securityParam      = sgSecurityParam sg
        !epochLength        = sgEpochLength sg
        !slotsPerKESPeriod  = sgSlotsPerKESPeriod sg
        !maxKESEvolutions   = sgMaxKESEvolutions sg
        !slotLength         = sgSlotLength sg
        !updateQuorum       = sgUpdateQuorum sg
        !maxLovelaceSupply  = sgMaxLovelaceSupply sg
        !protocolParams     = sgProtocolParams sg
        !genDelegs          = sgGenDelegs sg
        !initialFunds       = sgInitialFunds sg
        !staking            = sgStaking sg
    in Aeson.object
        [ "systemStart"       .= systemStart
        , "networkMagic"      .= networkMagic
        , "networkId"         .= networkId
        , "activeSlotsCoeff"  .= activeSlotsCoeff
        , "securityParam"     .= securityParam
        , "epochLength"       .= epochLength
        , "slotsPerKESPeriod" .= slotsPerKESPeriod
        , "maxKESEvolutions"  .= maxKESEvolutions
        , "slotLength"        .= slotLength
        , "updateQuorum"      .= updateQuorum
        , "maxLovelaceSupply" .= maxLovelaceSupply
        , "protocolParams"    .= protocolParams
        , "genDelegs"         .= genDelegs
        , "initialFunds"      .= initialFunds
        , "staking"           .= staking
        ]
  toEncoding sg =
    let !systemStart        = sgSystemStart sg
        !networkMagic       = sgNetworkMagic sg
        !networkId          = sgNetworkId sg
        !activeSlotsCoeff   = sgActiveSlotsCoeff sg
        !securityParam      = sgSecurityParam sg
        !epochLength        = sgEpochLength sg
        !slotsPerKESPeriod  = sgSlotsPerKESPeriod sg
        !maxKESEvolutions   = sgMaxKESEvolutions sg
        !slotLength         = sgSlotLength sg
        !updateQuorum       = sgUpdateQuorum sg
        !maxLovelaceSupply  = sgMaxLovelaceSupply sg
        !protocolParams     = sgProtocolParams sg
        !genDelegs          = sgGenDelegs sg
        !staking            = sgStaking sg
        !initialFunds       = sgInitialFunds sg
    in Aeson.pairs $ mconcat
        [ "systemStart"       .= systemStart
        , "networkMagic"      .= networkMagic
        , "networkId"         .= networkId
        , "activeSlotsCoeff"  .= activeSlotsCoeff
        , "securityParam"     .= securityParam
        , "epochLength"       .= epochLength
        , "slotsPerKESPeriod" .= slotsPerKESPeriod
        , "maxKESEvolutions"  .= maxKESEvolutions
        , "slotLength"        .= slotLength
        , "updateQuorum"      .= updateQuorum
        , "maxLovelaceSupply" .= maxLovelaceSupply
        , "protocolParams"    .= protocolParams
        , "genDelegs"         .= genDelegs
        , "staking"           .= staking
        , "initialFunds"      .= initialFunds
        ]

data OutputShelleyGenesisStaking era = OutputShelleyGenesisStaking
  { osgsPools :: !(ListMap (KeyHash 'Ledger.StakePool (Crypto era)) (Ledger.PoolParams         (Crypto era)))
  , osgsStake :: !(ListMap (KeyHash 'Ledger.Staking   (Crypto era)) (KeyHash 'Ledger.StakePool (Crypto era)))
  }
  deriving stock (Eq, Show, Generic)

instance
  ( Ledger.Crypto (Crypto era)
  ) => ToJSON (OutputShelleyGenesisStaking era) where
  toJSON sg =
    let !pools = osgsPools sg
        !stake = osgsStake sg
    in Aeson.object
        [ "pools" .= pools
        , "stake" .= stake
        ]
  toEncoding sg =
    let !pools       = osgsPools sg
        !stake       = osgsStake sg
    in Aeson.pairs $ mconcat
        [ "pools" .= pools
        , "stake" .= stake
        ]
