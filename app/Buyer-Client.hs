{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class                  (MonadIO (..))
import Data.Aeson                              (Result (..), fromJSON)
import Data.Monoid                             (Last (..))
import Data.Proxy                              (Proxy (..))
import Data.Text                               (pack)
import Data.UUID
import Ledger.Value                            (flattenValue)
import Network.HTTP.Req
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import Plutus.PAB.Webserver.Types
import System.Environment                      (getArgs)
import System.IO
import Text.Read                               (readMaybe)

import PSPAB                                   (PSContracts)

main :: IO ()
main = do
    [i :: Int] <- map read <$> getArgs
    uuid       <- read <$> readFile ('W' : show i ++ ".cid")
    hSetBuffering stdout NoBuffering
    putStrLn $ "User contract instance id for Wallet " ++ show i ++ ": " ++ show uuid
    go uuid
  where
    go :: UUID -> IO a
    go uuid = do
        cmd <- readCommand
        case cmd of
            BuyTokens amt -> offer uuid amt
        go uuid

    readCommand :: IO Command
    readCommand = do
        putStr "enter BuyTokens amt: "
        s <- getLine
        maybe readCommand return $ readMaybe s

data Command = BuyTokens Integer
    deriving (Show, Read, Eq, Ord)

buyTokens :: UUID -> Integer -> IO ()
buyTokens uuid amt = handle h $ runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "buyTokens")
        (ReqBodyJson amt)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "bought " ++ show amt ++ " Tokens"
        else "error buying tokens"
  where
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> offer uuid amt

