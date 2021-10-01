{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module MarketPlace where

import           Control.Monad                hiding (fmap)
import qualified Data.Map                     as Map
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Prelude                      (Semigroup (..), Show (..))
import qualified Prelude

import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency    as Currency
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)

import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value                 as Value

data Marketplace =
  Marketplace
    { mpOperator   :: !PubKeyHash
    , mpSaleFee    :: !Percentage -- percentage for selling Property Tokens
    , mpMintFee    :: !Integer
    , mpSymbol     :: !CurrencySymbol 
    , mpAssetClass :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Marketplace
PlutusTx.unstableMakeIsData ''Marketplace   
    
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

-- | The idea here is to create a secondary marketplace where the price is determined buy an oracle 
data MarketplaceRedeemer = Use 
   deriving Show

data MarketplaceParams = 
  MarketplaceParams 
     { mintFee  :: Integer 
     , saleFee  :: Ratio Integer
     } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)