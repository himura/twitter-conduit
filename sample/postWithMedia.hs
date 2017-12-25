{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Twitter.Conduit
import Common

import qualified Data.Text as T
import System.Environment

main :: IO ()
main = do
    [status, filepath] <- getArgs
    putStrLn $ "Post message: " ++ status
    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings
    res <- call twInfo mgr $ statusesUpdateWithMedia (T.pack status) (MediaFromFile filepath)
    print res
