{-# LANGUAGE ScopedTypeVariables        #-}

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
import           Ledger.Ada           as Ada
import           Playground.Contract  (ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Schema               (ToSchema)
import           Text.Printf          (printf)

data SwapDatum = SwapDatum
    { sRecipient   :: !PubKeyHash
    , sAmount   :: !Integer
    } deriving Show

PlutusTx.unstableMakeIsData ''SwapDatum

data Swapping
instance Scripts.ScriptType Swapping where
    type instance DatumType Swapping = SwapDatum
    type instance RedeemerType Swapping = ()

{-# INLINABLE mkValidator #-}
mkValidator :: SwapDatum -> () -> ValidatorCtx -> Bool
mkValidator sd _ _ = sAmount sd == 1

swappingInstance :: Scripts.ScriptInstance Swapping
swappingInstance = Scripts.validator @Swapping
        $$(PlutusTx.compile [|| mkValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @SwapDatum @()

validator :: Validator
validator = Scripts.validatorScript swappingInstance

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data BuyParms = BuyParms
    { bpAmount      :: !Integer
    , bpBid         :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data SellParams = SellParams
    { spAmount  :: !Integer
    , spBid     :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SealExchangeSchema = 
    BlockchainActions
        .\/ Endpoint "buy" BuyParms
        .\/ Endpoint "sell" SellParams

buy :: (HasBlockchainActions s, AsContractError e) => BuyParms -> Contract w s e ()
buy BuyParms{..} = do
    pkh <- pubKeyHash <$> ownPubKey
    utxos <- Map.filter f <$> utxoAt (ScriptAddress valHash)
    logInfo @String $ printf "%s" $ show utxos
    logInfo @String $ printf "bid %d lovelare for %d seal" bpBid bpAmount
    let s = SwapDatum
                { sRecipient =  pkh
                , sAmount    =  bpAmount}
        v = Ada.lovelaceValueOf bpBid
    let tx = mustPayToTheScript s v
    ledgerTx <- submitTxConstraints swappingInstance tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "bid %d lovelare for %d seal" bpBid bpAmount
   where
    f :: TxOutTx -> Bool
    f o = True

sell :: (HasBlockchainActions s, AsContractError e) => SellParams -> Contract w s e ()
sell SellParams{..} = do
    pkh <- pubKeyHash <$> ownPubKey
    let s = SwapDatum
                { sRecipient =  pkh
                , sAmount    =  spBid}
        v = Value.singleton "7365616c" "Seal" spAmount
    let tx = mustPayToTheScript s v
    ledgerTx <- submitTxConstraints swappingInstance tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "sell %d seal for %d lovelace" spAmount 

endpoints :: Contract () SealExchangeSchema Text ()
endpoints = (buy' `select` sell') >> endpoints
  where
    buy' = endpoint @"buy" >>= buy
    sell'   = endpoint @"sell"   >>= sell 

mkSchemaDefinitions ''SealExchangeSchema

tokenSeal :: KnownCurrency
tokenSeal = KnownCurrency (ValidatorHash "seal") "Token" (TokenName "Seal" :| [])

mkSchemaDefinitions ''BlockchainActions

$(mkKnownCurrencies ['tokenSeal])
