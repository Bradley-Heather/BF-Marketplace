{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module PropertySaleTest where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Ledger
import           Ledger.Ada                 as Ada
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, String, Show (..))

import           PropertySale
import           BoraMarket 


runBoraTrace :: IO ()
runBoraTrace = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(Wallet w, v) | w <- [1 .. 4]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 

myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet 4) startEndpoint
    callEndpoint @"Start" h BoraMarketParams
           { bmpListFee  = 5_000_000
           , bmpSaleFee  = 1_000_000
           }
    void $ Emulator.waitNSlots 5
    Last n <- observableState h 
    case n of 
      Nothing -> Extras.logError @String "Error starting marketplace"
      Just marketplace -> do 
          Extras.logInfo $ "Started marketplace " ++ show marketplace
          
          h1 <- activateContractWallet (Wallet 1) $ mintEndpoint marketplace
          callEndpoint @"Mint" h1 MintParams 
                { mpTokenName = "Seaside View"
                , mpAmount    = 200
                }
          void $ Emulator.waitNSlots 5
          Last m <- observableState h1
          case m of
              Nothing -> Extras.logError @String "Error starting property sale"
              Just ps -> do
                   Extras.logInfo $ "Started Property Sale " ++ show ps

                   h2 <- activateContractWallet (Wallet 1) $ sellEndpoints ps
                   h3 <- activateContractWallet (Wallet 2) $ buyEndpoint ps
                   h4 <- activateContractWallet (Wallet 3) $ buyEndpoint ps

                   callEndpoint @"List Property" h2 10_000_000 
                   void $ Emulator.waitNSlots 5

                   callEndpoint @"Buy Tokens" h3 40
                   void $ Emulator.waitNSlots 5

                   callEndpoint @"Buy Tokens" h4 50
                   void $ Emulator.waitNSlots 5

                   callEndpoint @"Withdraw Funds" h2 10_000_000
                   void $ Emulator.waitNSlots 5

                   callEndpoint @"Close" h2 ()
                   void $ Emulator.waitNSlots 2

                   callEndpoint @"Buy Tokens" h4 50
                   void $ Emulator.waitNSlots 5

-------------------------------------------------------------

-- To Do -- Quickcheck and lenses