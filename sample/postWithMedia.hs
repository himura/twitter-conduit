{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Twitter.Conduit
import Common

import qualified Data.Text as T
import Network.HTTP.Conduit
import System.Environment

main :: IO ()
main = do
    [status, filepath] <- getArgs
    putStrLn $ "Post message: " ++ status
    twInfo <- getTWInfoFromEnv
    res <- withManager $ \mgr -> do
        call twInfo mgr $ updateWithMedia (T.pack status) (MediaFromFile filepath)
    print res
