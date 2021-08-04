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

scrAddress :: Ledger.Address
scrAddress = scriptAddress sealSwapValidator

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
    logInfo @String $ printf "sealSell %d seal for %d lovelace" spAmount 

-- Buy
data SealBuyParms = SealBuyParms
    { bpAmount      :: !Integer
    , bpBid         :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


sealBuy :: (HasBlockchainActions s, AsContractError e) => SealBuyParms -> Contract w s e ()
sealBuy SealBuyParms{..} = do
    pkh <- pubKeyHash <$> ownPubKey
    utxos <- Map.filter f <$> utxoAt (ScriptAddress sealSwapValidatorHash)
    logInfo @String $ printf "%s" $ show utxos
    logInfo @String $ printf "bid %d lovelare for %d seal" bpBid bpAmount
    let s = SealSwapDatum
                { sRecipient =  pkh
                , sAmount    =  bpAmount}
        v = lovelaceValueOf bpBid
    let tx = mustPayToTheScript s v
    ledgerTx <- submitTxConstraints sealSwapInstance tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "bid %d lovelare for %d seal" bpBid bpAmount
   where
    f :: TxOutTx -> Bool
    f o = True

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