{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module BotPlutusInterface.Types (
  PABConfig (..),
  CLILocation (..),
  AppState (AppState),
  LogLevel (..),
  ContractEnvironment (..),
  Tip (Tip, epoch, hash, slot, block, era, syncProgress),
  ContractState (..),
  SomeContractState (SomeContractState),
  HasDefinitions (..),
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
  RawTx (..),
) where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Cardano.Api.ProtocolParameters (ProtocolParameters)
import Control.Concurrent.STM (TVar)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.TH (Options (..), defaultOptions, deriveJSON)
import Data.Default (Default (def))
import Data.Kind (Type)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (PubKeyHash)
import Ledger.TimeSlot (SlotConfig)
import Network.Wai.Handler.Warp (Port)
import Numeric.Natural (Natural)
import Plutus.PAB.Core.ContractInstance.STM (Activity)
import Plutus.PAB.Effects.Contract.Builtin (
  HasDefinitions (..),
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
 )
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import Wallet.Types (ContractInstanceId (..))
import Prelude

data PABConfig = PABConfig
  { -- | Calling the cli through ssh when set to Remote
    pcCliLocation :: !CLILocation
  , pcChainIndexUrl :: !BaseUrl
  , pcNetwork :: !NetworkId
  , pcProtocolParams :: !ProtocolParameters
  , -- | Slot configuration of the network, the default value can be used for the mainnet
    pcSlotConfig :: !SlotConfig
  , -- | Directory name of the script and data files
    pcScriptFileDir :: !Text
  , -- | Directory name of the signing key files
    pcSigningKeyFileDir :: !Text
  , -- | Directory name of the transaction files
    pcTxFileDir :: !Text
  , -- | Protocol params file location relative to the cardano-cli working directory (needed for the cli)
    pcProtocolParamsFile :: !Text
  , -- | Directory name of metadata files
    pcMetadataDir :: !Text
  , -- | Dry run mode will build the tx, but skip the submit step
    pcDryRun :: !Bool
  , pcLogLevel :: !LogLevel
  , pcOwnPubKeyHash :: !PubKeyHash
  , pcTipPollingInterval :: !Natural
  , pcPort :: !Port
  , pcEnableTxEndpoint :: !Bool
  }
  deriving stock (Show, Eq)

data ContractEnvironment w = ContractEnvironment
  { cePABConfig :: PABConfig
  , ceContractInstanceId :: ContractInstanceId
  , ceContractState :: TVar (ContractState w)
  }
  deriving stock (Show)

data Tip = Tip
  { epoch :: Integer
  , hash :: Text
  , slot :: Integer
  , block :: Integer
  , era :: Text
  , syncProgress :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (JSON.FromJSON)

instance Show (TVar (ContractState w)) where
  show _ = "<ContractState>"

newtype AppState = AppState (TVar (Map ContractInstanceId SomeContractState))

{- | This type is wrapping a ContractState in a TVar and existentially quantifying the @w@
 type variable, so we can store different contracts in the AppState
-}
data SomeContractState
  = forall (w :: Type). (ToJSON w) => SomeContractState (TVar (ContractState w))

data ContractState w = ContractState
  { csActivity :: Activity
  , csObservableState :: w
  }
  deriving stock (Show, Eq)

data CLILocation = Local | Remote Text
  deriving stock (Show, Eq)

data LogLevel = Error | Warn | Notice | Info | Debug
  deriving stock (Eq, Ord, Show)

instance Default PABConfig where
  def =
    PABConfig
      { pcCliLocation = Local
      , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
      , pcNetwork = Testnet (NetworkMagic 42)
      , pcProtocolParams = def
      , pcSlotConfig = def
      , pcTipPollingInterval = 10_000_000
      , pcScriptFileDir = "./result-scripts"
      , pcSigningKeyFileDir = "./signing-keys"
      , pcTxFileDir = "./txs"
      , pcMetadataDir = "/metadata"
      , pcDryRun = True
      , pcProtocolParamsFile = "./protocol.json"
      , pcLogLevel = Info
      , pcOwnPubKeyHash = ""
      , pcPort = 9080
      , pcEnableTxEndpoint = False
      }

data RawTx = RawTx
  { _type :: Text
  , _description :: Text
  , _cborHex :: Text
  }
  deriving (Generic, Eq, Show)

-- type is a reserved keyword in haskell and can not be used as a field name
-- when converting this to JSON we drop the _ prefix from each field
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''RawTx
