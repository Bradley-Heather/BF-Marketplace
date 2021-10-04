{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           Control.Monad                       (forM_, void, when)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, ToJSON, Result (..), fromJSON)
import           Data.Default                        (Default (..))
import           Data.Monoid                         (Last (..))
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)

import           Plutus.Contract
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server

import           Wallet.Emulator.Types               (Wallet (..))
import           Wallet.Types                        (ContractInstanceId (..))

import qualified PropertySale                        as PS
import qualified BoraMarket                          as BM

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 6]]

bmp :: BM.BoraMarketParams
bmp = BM.BoraMarketParams
           { BM.bmpListFee  = 5_000_000
           , BM.bmpSaleFee  = 1_000_000
           }

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin PSContracts) "Starting Property Sale PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidMarket <- Simulator.activateContract (Wallet 6) BMStart 
    liftIO $ writeFile "Market.cid" $ show $ unContractInstanceId cidMarket
    _  <- Simulator.callEndpointOnInstance cidMarket "Start" bmp
    bm <- waitForLast cidMarket 

    cidMinter <- Simulator.activateContract (Wallet 1) $ PSMinter bm
    liftIO $ writeFile "Minter.cid" $ show $ unContractInstanceId cidMinter
    ps <- waitForLast cidMinter

    cidSeller <- Simulator.activateContract (Wallet 1) $ PSSeller ps
    liftIO $ writeFile "Seller.cid" $ show $ unContractInstanceId cidSeller

    forM_ wallets $ \w ->
        when (w /= Wallet 1) $ do
            cid <- Simulator.activateContract w $ PSBuyer ps
            liftIO $ writeFile ("Buyer" ++ show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid

    void $ liftIO getLine
    shutdown       

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing

data PSContracts = BMStart | PSMinter BM.BoraMarket | PSSeller PS.PropertySale | PSBuyer PS.PropertySale
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Pretty PSContracts where
    pretty = viaShow

instance Builtin.HasDefinitions PSContracts where
    getDefinitions = [BMStart]
    getSchema = \case
        BMStart         -> Builtin.endpointsToSchemas @BM.BoraMarketSchema
        PSMinter _      -> Builtin.endpointsToSchemas @PS.PSMintSchema
        PSSeller _      -> Builtin.endpointsToSchemas @PS.PSSellSchema
        PSBuyer _       -> Builtin.endpointsToSchemas @PS.PSBuySchema
    getContract = \case
        BMStart         -> SomeBuiltin BM.startEndpoint
        PSMinter bm     -> SomeBuiltin $ PS.mintEndpoint bm
        PSSeller ps     -> SomeBuiltin $ PS.sellEndpoints ps 
        PSBuyer ps      -> SomeBuiltin $ PS.buyEndpoint ps

handlers :: SimulatorEffectHandlers (Builtin PSContracts)
handlers =
     Simulator.mkSimulatorHandlers def def
    $ interpret (Builtin.contractHandler (Builtin.handleBuiltin @PSContracts))