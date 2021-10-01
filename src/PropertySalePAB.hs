{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}

module PropertySalePAB
    ( PSContracts (..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)
import           Ledger

import qualified PropertySale               

data PSContracts = Mint | Interact PropertySale
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty PSContracts where
    pretty = viaShow