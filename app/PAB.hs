{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
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
import           Data.Aeson                          (FromJSON, Result (..), fromJSON)
import           Data.Default                        (Default (..))
import           Data.Monoid                         (Last (..))

import           Plutus.Contract
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server

import           Wallet.Emulator.Types               (Wallet (..))
import           Wallet.Types                        (ContractInstanceId (..))

import qualified PropertySale                        as PS
import           PropertySalePAB

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 5]]

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin PSContracts) "Starting Property Sale PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidMinter <- Simulator.activateContract (Wallet 1) PSMinter
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

instance Builtin.HasDefinitions PSContracts where
    getDefinitions = [PSMinter]
    getSchema = \case
        PSMinter        -> Builtin.endpointsToSchemas @PS.PSMintSchema
        PSSeller _      -> Builtin.endpointsToSchemas @PS.PSSellSchema
        PSBuyer _       -> Builtin.endpointsToSchemas @PS.PSBuySchema
    getContract = \case
        PSMinter        -> SomeBuiltin PS.mintEndpoint
        PSSeller ps     -> SomeBuiltin $ PS.sellEndpoints ps 
        PSBuyer ps      -> SomeBuiltin $ PS.buyEndpoint ps

handlers :: SimulatorEffectHandlers (Builtin PSContracts)
handlers =
     Simulator.mkSimulatorHandlers def def
    $ interpret (Builtin.contractHandler (Builtin.handleBuiltin @PSContracts))