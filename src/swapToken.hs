{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract      hiding (when)
import           PlutusTx             (Data (..))
import qualified PlutusTx             as PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           (lovelaceValueOf)
import           Playground.Contract  (ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Schema               (ToSchema)
import           Text.Printf          (printf)

-- | Define Token
{-# INLINABLE sealSymbol #-}
-- | The 'CurrencySymbol' of the 'Seal' currency.
sealSymbol :: CurrencySymbol
sealSymbol = Value.currencySymbol "Seal"

{-# INLINABLE sealToken #-}
-- | The 'TokenName' of the 'Seal' currency.
sealToken :: TokenName
sealToken = Value.tokenName "Seal" 

{-# INLINABLE sealValueOf #-}
-- | A 'Value' with the given amount of Seal (the currency unit).
sealValueOf :: Integer -> Value
sealValueOf = Value.singleton sealSymbol sealToken


tokenSeal :: KnownCurrency
tokenSeal = KnownCurrency
    { hash = Ledger.fromSymbol sealSymbol
    , friendlyName = "Seal"
    , knownTokens = sealToken :| []
    }

-- | Contract Code

-- Data
data SealSwapDatum = SealSwapDatum
    { sRecipient   :: !PubKeyHash
    , sAmount   :: !Integer
    } deriving Show

PlutusTx.unstableMakeIsData ''SealSwapDatum
PlutusTx.makeLift ''SealSwapDatum

data SealSwapAction = BuyAction | CloseAction
    deriving Show

PlutusTx.unstableMakeIsData ''SealSwapAction
PlutusTx.makeLift ''SealSwapAction

data SealSwapping
instance Scripts.ScriptType SealSwapping where
    type instance DatumType SealSwapping = SealSwapDatum
    type instance RedeemerType SealSwapping = SealSwapAction

{-# INLINABLE mkSealSwapValidator #-}
mkSealSwapValidator :: SealSwapDatum -> SealSwapAction -> ValidatorCtx -> Bool
mkSealSwapValidator sd redeemer ctx =
    trace "mkSealSwapValidator" True

sealSwapInstance :: Scripts.ScriptInstance SealSwapping
sealSwapInstance = Scripts.validator @SealSwapping
        $$(PlutusTx.compile [|| mkSealSwapValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @SealSwapDatum @SealSwapAction

sealSwapValidator :: Validator
sealSwapValidator = Scripts.validatorScript sealSwapInstance

sealSwapValidatorHash :: Ledger.ValidatorHash
sealSwapValidatorHash = Scripts.validatorHash sealSwapValidator

sealSwapScriptAddress :: Ledger.Address
sealSwapScriptAddress = scriptAddress sealSwapValidator

-- | Wallet Code
-- Sell
data SealSellParams = SealSellParams
    { spAmount  :: !Integer
    , spBid     :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

sealSell :: (HasBlockchainActions s, AsContractError e) => SealSellParams -> Contract w s e ()
sealSell SealSellParams{..} = do
    pkh <- pubKeyHash <$> ownPubKey
    let s = SealSwapDatum
                { sRecipient =  pkh
                , sAmount    =  spBid}
    let tx = mustPayToTheScript s $ sealValueOf spAmount
    ledgerTx <- submitTxConstraints sealSwapInstance tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "sealSell %d seal for %d lovelace" spAmount spBid

-- Buy
data SealBuyParms = SealBuyParms
    { bpAmount      :: !Integer
    , bpBid         :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


sealBuy :: forall w s. HasBlockchainActions s => SealBuyParms -> Contract w s Text ()
sealBuy SealBuyParms{..} = do
    (oref, o, d) <- findUtxo sealSymbol sealToken bpAmount
    logInfo @String $ printf "found seal swap utxo with datum %s" (show d)

    pkh <- pubKeyHash <$> ownPubKey
    logInfo @String $ printf "bid %d lovelare for %d seal" bpBid bpAmount
    let v  = sealValueOf bpAmount <> lovelaceValueOf bpBid
        d' = d
        r  = Redeemer $ PlutusTx.toData BuyAction
        lookups = Constraints.scriptInstanceLookups sealSwapInstance
               <> Constraints.otherScript sealSwapValidator 
               <> Constraints.unspentOutputs (Map.singleton oref o)
        tx = mustPayToTheScript d' v
          <> mustPayToPubKey (sRecipient d) (lovelaceValueOf bpBid)
          <> mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "bid %d lovelare for %d seal" bpBid bpAmount

findUtxo :: HasBlockchainActions s
         => CurrencySymbol
         -> TokenName
         -> Integer
         -> Contract w s Text (TxOutRef, TxOutTx, SealSwapDatum)
findUtxo cs tn amount = do
    utxos <- utxoAt sealSwapScriptAddress
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (txOutValue $ txOutTxOut o) cs tn == amount
             ]
    case xs of
        [(oref, o)] -> case txOutType $ txOutTxOut o of
            PayToPubKey   -> throwError "unexpected out type"
            PayToScript h -> case Map.lookup h $ txData $ txOutTxTx o of
                Nothing        -> throwError "datum not found"
                Just (Datum e) -> case PlutusTx.fromData e of
                    Nothing -> throwError "datum has wrong type"
                    Just d@SealSwapDatum{..}
                        | sAmount > 0 -> return (oref, o, d)
                        | otherwise   -> throwError "seal swap token invalid"
        _           -> throwError "seal swap utxo not found"

-- wallet binding
type SealExchangeSchema = 
    BlockchainActions
        .\/ Endpoint "sealBuy" SealBuyParms
        .\/ Endpoint "sealSell" SealSellParams

mkSchemaDefinitions ''SealExchangeSchema
mkSchemaDefinitions ''BlockchainActions

endpoints :: Contract () SealExchangeSchema Text ()
endpoints = (sealBuy' `select` sealSell') >> endpoints
  where
    sealSell'   = endpoint @"sealSell"   >>= sealSell 
    sealBuy' = endpoint @"sealBuy" >>= sealBuy

$(mkKnownCurrencies ['tokenSeal])