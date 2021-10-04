{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module PropertySale where

import           Control.Monad                hiding (fmap)
-- import qualified Data.Map                     as Map
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Prelude                      (Semigroup (..), Show (..), (<$>))
import qualified Prelude
import qualified Schema

import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency    as Currency
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless, (<$>))

import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value                 as Value

import           BoraMarket                   as BM 


data MintParams = 
  MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Show, Generic, ToJSON, FromJSON, Prelude.Eq, Schema.ToSchema)

PlutusTx.makeLift ''MintParams

data PropertySale = 
  PropertySale
    { psOperator         :: !PubKeyHash
    , psOperatorListFee  :: !Value
    , psOperatorSaleFee  :: !Value
    , psSeller           :: !PubKeyHash
    , psToken            :: !AssetClass
    , psName             :: !TokenName
    , psAmount           :: !Integer
    , psTT               :: !(Maybe ThreadToken)
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''PropertySale

type TokenPrice     = Integer
type TokenAmount    = Integer
type FundsAmount    = Integer

data PropertySaleRedeemer = 
      ListProperty       TokenPrice  
    | WithdrawTokens     TokenAmount 
    | WithdrawFunds      FundsAmount
    | Close
    | BuyTokens          TokenAmount
    deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''PropertySaleRedeemer

data TradeDatum = Trade Integer | Finished
    deriving Show

PlutusTx.unstableMakeIsData ''TradeDatum

--------------------------------------------------------------------------
-- | OnChain code | --

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

---------------------------------------
-- | State Machine

{-# INLINABLE transition #-}
transition :: PropertySale -> State TradeDatum 
              -> PropertySaleRedeemer -> Maybe (TxConstraints Void Void, State TradeDatum)
transition ps s r = case (stateValue s, stateData s, r) of
    (v, Trade _, ListProperty p) 
      | p >= 0             -> Just ( Constraints.mustPayToPubKey (psOperator ps) (psOperatorListFee ps) <>
                                     Constraints.mustBeSignedBy (psSeller ps)
                                   , State (Trade p) $ v                     <> 
                                     assetClassValue (psToken ps) (psAmount ps)
                                   )
    (v, Trade p, WithdrawTokens n ) 
      | n >= 0             -> Just ( Constraints.mustBeSignedBy (psSeller ps)
                                   , State (Trade p) $ v                     <>
                                     assetClassValue (psToken ps) (negate n) 
                                   )
    (v, Trade p, WithdrawFunds f) 
      | f > 0              -> Just ( Constraints.mustBeSignedBy (psSeller ps)
                                   , State (Trade p) $ v                     <>
                                     lovelaceValueOf (negate f)
                                   )                               
    (_, Trade _, Close)    -> Just ( Constraints.mustBeSignedBy (psSeller ps)
                                   , State Finished mempty
                                   )
    (v, Trade p, BuyTokens n)  
      | n > 0              -> Just ( Constraints.mustPayToPubKey (psOperator ps) (psOperatorSaleFee ps)
                                   , State (Trade p) $ v                     <> 
                                     assetClassValue (psToken ps) (negate n) <>
                                     lovelaceValueOf (n * p)
                                   )                               
    _                      -> Nothing

{-# INLINABLE final #-}
final :: TradeDatum -> Bool
final Finished = True
final _        = False

{-# INLINABLE psStateMachine #-}
psStateMachine :: PropertySale -> StateMachine TradeDatum PropertySaleRedeemer
psStateMachine ps = mkStateMachine (psTT ps) (transition ps) final -- final sepcifies final state of the state machine

{-# INLINABLE mkPSValidator #-}
mkPSValidator :: PropertySale -> TradeDatum -> PropertySaleRedeemer -> ScriptContext -> Bool
mkPSValidator = mkValidator . psStateMachine

type PS = StateMachine TradeDatum PropertySaleRedeemer

psTypedValidator :: PropertySale -> Scripts.TypedValidator PS
psTypedValidator ps = Scripts.mkTypedValidator @PS
    ($$(PlutusTx.compile [|| mkPSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ps)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TradeDatum @PropertySaleRedeemer

psValidator :: PropertySale  -> Validator
psValidator = Scripts.validatorScript . psTypedValidator

psAddress :: PropertySale  -> Ledger.Address
psAddress = scriptAddress . psValidator

-- | Allows for the starting and stepping of the state machine
psClient :: PropertySale  -> StateMachineClient TradeDatum PropertySaleRedeemer
psClient ps = mkStateMachineClient $ StateMachineInstance (psStateMachine ps) (psTypedValidator ps)

---------------------------------------------------------------------------
-- | Offchain Code | --

-- | Starts the Property Sale by initialising the state machine and minting the required number of tokens using a One Shot policy
startPS :: BM.BoraMarket -> MintParams -> Contract (Last PropertySale) s Text ()
startPS bm mp = do
    pkh <- pubKeyHash <$> ownPubKey
    minted <- mapError (pack . show) 
              (mintContract pkh [(mpTokenName mp, mpAmount mp)] :: Contract w s CurrencyError OneShotCurrency)
    tt  <- Just <$> mapErrorSM getThreadToken 
    let cs = Currency.currencySymbol minted
        ps = PropertySale
            { psOperator         = bmOperator bm
            , psOperatorListFee  = lovelaceValueOf (bmListFee bm)  
            , psOperatorSaleFee  = lovelaceValueOf (bmSaleFee bm)
            , psSeller           = pkh
            , psToken            = AssetClass (cs, mpTokenName mp) 
            , psName             = mpTokenName mp
            , psAmount           = mpAmount mp
            , psTT               = tt
            }
        client = psClient ps
    void $ mapErrorSM $ runInitialise client (Trade 0) mempty
    tell $ Last $ Just ps
    logInfo $ "Started property sale " ++ show ps
    logInfo $ "Property listed as: " ++ show (mpTokenName mp) ++ ", Total tokens distributed: " ++ show (mpAmount mp)

-- | Converts SMContractError from the state machine to a simple text error
mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

---------------------------------------
-- | Seller Actions
listProperty :: PropertySale -> TokenPrice -> Contract w s Text ()
listProperty ps p = do
      void $ mapErrorSM $ runStep (psClient ps) $ ListProperty p 
      logInfo $ show (psAmount ps) ++ " " ++ show (psName ps) ++ " tokens listed for " ++ show p

withdrawTokens :: PropertySale -> TokenAmount -> Contract w s Text ()
withdrawTokens ps n = do
      void $ mapErrorSM $ runStep (psClient ps) $ WithdrawTokens n 
      if n > 0 then   
        logInfo $ show n ++ " " ++ show (psName ps) ++ " tokens withdrawn" 
      else logInfo $ "No " ++ show (psName ps) ++ " tokens withdrawn"

withdrawFunds :: PropertySale -> FundsAmount -> Contract w s Text ()
withdrawFunds ps f = do
      void $ mapErrorSM $ runStep (psClient ps) $ WithdrawFunds f
      logInfo $ show f ++ " Funds withdrawn"

close :: PropertySale -> Contract w s Text ()
close ps = do
      void $ mapErrorSM $ runStep (psClient ps) Close 
      logInfo $ show (psName ps) ++ " sale closed"

-- | Buyer Actions
buyTokens :: PropertySale -> TokenAmount -> Contract w s Text ()
buyTokens ps n = do 
      void $ mapErrorSM $ runStep (psClient ps) $ BuyTokens n
      logInfo $ show n ++ " " ++ show (psName ps) ++ " tokens purchased"

---------------------------------------
{-
checkBalance :: Contract w s Text ()
checkBalance = do 
     pk    <- Contract.ownPubKey 
     utxos <- utxoAt $ pubKeyAddress pk 
     let v = mconcat$ Map.elems $ txOutValue . txOutTxOut <$> utxos
     logInfo @String $ "Current balance: " ++ show (Value.flattenValue v)
     return v
-}
          
type PSMintSchema =
        Endpoint "Mint"             MintParams
type PSSellSchema =
        Endpoint "List Property"    TokenPrice
    .\/ Endpoint "Withdraw Tokens"  TokenAmount 
    .\/ Endpoint "Withdraw Funds"   FundsAmount
    .\/ Endpoint "Close"            ()
type PSBuySchema =  
        Endpoint "Buy Tokens"       TokenAmount

mintEndpoint :: BM.BoraMarket -> Contract (Last PropertySale) PSMintSchema Text ()
mintEndpoint bm = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"Mint" $ startPS bm

sellEndpoints :: PropertySale  -> Contract () PSSellSchema Text ()
sellEndpoints ps = forever
                $ handleError logError
                $ awaitPromise
                $ listProperty' `select` withdrawTokens' `select` withdrawFunds' `select` close'
  where
    listProperty'   = endpoint @"List Property"   $ listProperty ps
    withdrawTokens' = endpoint @"Withdraw Tokens" $ withdrawTokens ps
    withdrawFunds'  = endpoint @"Withdraw Funds"  $ withdrawFunds ps
    close'          = endpoint @"Close"           $ const $ close ps

buyEndpoint :: PropertySale  -> Contract () PSBuySchema Text ()
buyEndpoint ps = forever
                $ handleError logError
                $ awaitPromise
                $ endpoint @"Buy Tokens" $ buyTokens ps

-- | To Do figure out how to incorporate funds check

