{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Twitter.Conduit
import Common

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

main :: IO ()
main = do
    status <- T.concat . map T.pack <$> getArgs
    T.putStrLn $ "Post message: " <> status
    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings
    res <- call twInfo mgr $ statusesUpdate status
    print res
