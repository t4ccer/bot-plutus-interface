{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.PlutusExample.NFT where

import Prelude qualified as Hask
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import Control.Monad hiding (fmap)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Map qualified as Map
import Data.Monoid (Last (Last))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import Ledger.Typed.Scripts (wrapMintingPolicy)
import Ledger.Value as Value
import Plutus.Contract (Contract, Endpoint, submitTxConstraintsWith, tell, utxosAt)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless, decodeUtf8)
import Text.Printf (printf)
import Prelude (Semigroup (..), String, show)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import Cardano.Prelude (fromRight, decodeUtf8')
import Data.ByteString.Base16 (encode)
import Plutus.Contracts.Currency (CurrencyError, mintContract)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> BuiltinData -> ScriptContext -> Bool
mkPolicy _ _ _ _ = True
-- mkPolicy oref tn _ ctx =
--   traceIfFalse "UTxO not consumed" hasUTxO
--     && traceIfFalse "wrong amount minted" checkMintedAmount
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     hasUTxO :: Bool
--     hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

--     checkMintedAmount :: Bool
--     checkMintedAmount = case flattenValue (txInfoMint info) of
--       [(cs, tn', amt)] -> cs == ownCurrencySymbol ctx && tn' == tn && amt == 1
--       _ -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\oref' tn' -> wrapMintingPolicy $ mkPolicy oref' tn'||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode tn

policyScript :: TxOutRef -> TokenName -> Script
policyScript oref tn = Scripts.unMintingPolicyScript $ policy oref tn

policySBS :: TxOutRef -> TokenName -> SBS.ShortByteString
policySBS oref tn = SBS.toShort . LBS.toStrict $ serialise $ policyScript oref tn

policySerialised :: TxOutRef -> TokenName -> PlutusScript PlutusScriptV1
policySerialised oref tn = PlutusScriptSerialised $ policySBS oref tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

type NFTSchema =
  Endpoint "mint" TokenName

data NftMetadata = NftMetadata
  { policyId :: CurrencySymbol
  , name :: BuiltinByteString
  , image :: BuiltinByteString
  }

instance ToJSON NftMetadata where
  toJSON NftMetadata{..} =
    object
    [ "721" .= object
      [ toHex (unCurrencySymbol policyId) .= object
        [ toHex name .= object
          [ "name" .= toText name
          , "image" .= toText image
          ]
        ]
      ]
    ]
    where
      decodeUtf8 x = fromRight (Hask.error $ Hask.show x) $ decodeUtf8' x
      
      toHex (BuiltinByteString str) = decodeUtf8 (encode str)

      toText (BuiltinByteString str) = decodeUtf8 str

mintNft :: TokenName -> Contract (Last Text) NFTSchema Text ()
mintNft tn = do
  pkh <- Contract.ownPaymentPubKeyHash
  void $ Contract.mapError (Text.pack . Hask.show @CurrencyError) (mintContract pkh [(tn, 1)])
  
  -- utxos <- utxosAt (pubKeyHashAddress pkh Nothing)
  -- tell $ Last $ Just "Contract started with "
  -- case Map.keys utxos of
  --   [] -> Contract.logError @String "no utxo found"
  --   oref : _ -> do
  --     tell $ Last $ Just $ "Using oref:" <> Text.pack (show oref)
  --     let cs = curSymbol oref tn
  --         val = Value.singleton cs tn 1
  --         lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
  --         tx = Constraints.mustMintValue val
  --              <> Constraints.mustSpendPubKeyOutput oref
  --              -- <> Constraints.mustIncludeMetadata (NftMetadata cs (unTokenName tn) "")
  --              -- "{\"721\": {}}"
  --     void $ submitTxConstraintsWith @Void lookups tx
  --     Contract.logInfo @String $ printf "forged %s" (show val)
  --     tell $ Last $ Just "Finished"
