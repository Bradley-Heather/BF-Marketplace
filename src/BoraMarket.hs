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

module BoraMarket where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Prelude                      (Show (..))
import qualified Prelude
import qualified Schema

import           Plutus.Contract              as Contract
import           Plutus.Contracts.Currency    as Currency
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)

import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada

data BoraMarket =
  BoraMarket
    { bmSymbol     :: !CurrencySymbol
    , bmOperator   :: !PubKeyHash
    , bmListFee    :: !Integer
    , bmSaleFee    :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''BoraMarket
    
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

data BoraMarketParams = 
  BoraMarketParams 
     { bmpListFee    :: !Integer 
     , bmpSaleFee    :: !Integer
     } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Schema.ToSchema)

startBoraMarket :: BoraMarketParams -> Contract (Last BoraMarket) s Text ()
startBoraMarket bmp = do 
    pkh <- pubKeyHash <$> ownPubKey
    osc <- mapError (pack . show) (mintContract pkh [("Bora Token", 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        bm = BoraMarket 
           { bmSymbol   = cs
           , bmOperator = pkh 
           , bmListFee  = bmpListFee bmp
           , bmSaleFee  = bmpSaleFee bmp 
           }
    tell $ Last $ Just bm
    logInfo $ "Started Marketplace: " ++ show bm

type BoraMarketSchema = 
        Endpoint "Start"  BoraMarketParams

startEndpoint :: Contract (Last BoraMarket) BoraMarketSchema Text ()
startEndpoint = forever
                $ handleError logError
                $ awaitPromise
                $ endpoint @"Start" startBoraMarket 