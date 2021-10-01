{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module MarketPlace where

import qualified Data.Aeson              
import           GHC.Generics            (Generic)
import           Ledger
import           Plutus.Types.Percentage (Percentage)
import qualified PlutusTx
import qualified Prelude                  

data Marketplace =
  Marketplace
    { mpOperator :: !PubKeyHash,
      mpSaleFee  :: !Percentage -- percentage for selling Property Tokens
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)
    
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

-- | The idea here is to create a secondary marketplace where the price is determined buy an oracle 