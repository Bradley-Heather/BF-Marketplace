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
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract
import           Plutus.Contracts.Currency
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Semigroup (..), Show (..))
import qualified Prelude

data PropertySale = PropertySale
    { psSeller :: !PubKeyHash
    , psToken  :: !AssetClass
    , psTT     :: !(Maybe ThreadToken)
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''PropertySale

data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer
    | Close
    deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer

data TradeDatum = Trade Integer | Finished
    deriving Show

PlutusTx.unstableMakeIsData ''TradeDatum

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE transition #-}
transition :: PropertySale -> State TradeDatum -> TSRedeemer -> Maybe (TxConstraints Void Void, State TradeDatum)
transition ps s r = case (stateValue s, stateData s, r) of
    (v, Trade _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (psSeller ts)
                                                    , State (Trade p) v
                                                    )
    (v, Trade p, AddTokens n)  | n > 0            -> Just ( Constraints.mustBeSignedBy (psSeller ts)
                                                    , State (Trade p) $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) n
                                                    )
    (v, Trade p, BuyTokens n)  | n > 0            -> Just ( mempty
                                                    , State (Trade p) $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (n * p)
                                                    )
    (v, Trade p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (psSeller ts)
                                                    , State (Trade p) $
                                                      v                                       <>
                                                      assetClassValue (psToken ts) (negate n) <>
                                                      lovelaceValueOf (negate l)
                                                    )
    (v, Trade p, Close)                           -> Just  ( Constraints.mustBeSignedBy (psSeller ts)
                                                    , State Finished mempty
                                                    )
    _                                             -> Nothing

{-# INLINABLE final #-}
final :: TradeDatum -> Bool
final Finished = True
final _        = False

{-# INLINABLE tsStateMachine #-}
psStateMachine :: PropertySale -> StateMachine TradeDatum TSRedeemer
psStateMachine ps = mkStateMachine (psTT ps) (transition ps) final -- final sepcifies final state of the state machine

{-# INLINABLE mkTSValidator #-}
mkTSValidator :: PropertySale -> TradeDatum -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . psStateMachine

type PS = StateMachine TradeDatum PSRedeemer

tsTypedValidator :: PropertySale -> Scripts.TypedValidator PS
tsTypedValidator ps = Scripts.mkTypedValidator @PS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ps)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TradeDatum @PSRedeemer

tsValidator :: PropertySale  -> Validator
tsValidator = Scripts.validatorScript . tsTypedValidator

tsAddress :: PropertySale  -> Ledger.Address
tsAddress = scriptAddress . tsValidator

tsClient :: PropertySale  -> StateMachineClient TradeDatum TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsTypedValidator ts)

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

-- mintC :: TokenName -> Integer -> Contract w s CurrencyError OneShotCurrency
-- mintC token amt = mapErrorSM (mintContract pkh [(token, amt)])

startTS :: AssetClass -> Bool -> Contract (Last PropertySale) s Text ()
startTS token useTT = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    tt  <- if useTT then Just <$> mapErrorSM getThreadToken else return Nothing
    let ts = PropertySale
            { psSeller = pkh
            , psToken  = token
            , psTT     = tt
            }
        client = tsClient ts
    void $ mapErrorSM $ runInitialise client (Trade 0) mempty
    tell $ Last $ Just ts
    logInfo $ "started token sale " ++ show ts

interact :: PropertySale  -> TSRedeemer -> Contract w s Text ()
interact ts r = void $ mapErrorSM $ runStep (tsClient ts) r

type TSStartSchema =
        Endpoint "start"      (CurrencySymbol, TokenName, Bool)
type TSUseSchema =
        Endpoint "interact"   TSRedeemer
 

startEndpoint :: Contract (Last PropertySale ) TSStartSchema Text ()
startEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"start" $ \(cs, tn, useTT) -> startTS (AssetClass (cs, tn)) useTT

useEndpoints :: PropertySale  -> Contract () TSUseSchema Text ()
useEndpoints ts = forever
                $ handleError logError
                $ awaitPromise
                $ endpoint @"interact"  $ interact ts