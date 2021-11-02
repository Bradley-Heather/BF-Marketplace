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

module Percentage where 

import           Data.Aeson          (ToJSON, FromJSON)
import           GHC.Generics        (Generic)
import           Prelude      hiding (Fractional)

import qualified PlutusTx

type Fractional = (Integer, Integer)

newtype Percentage =
      Percentage
      {getPercentage :: Fractional}
      deriving (Eq, Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''Percentage
PlutusTx.unstableMakeIsData ''Percentage

mkPercentage :: Fractional -> Maybe Percentage
mkPercentage percentage@(numerator, denominator) =
      let roundedPercentage = abs $ numerator `div` denominator
      in
      if denominator /= 0 && 0 <= roundedPercentage && roundedPercentage <= 100
            then pure $ Percentage percentage
            else Nothing
