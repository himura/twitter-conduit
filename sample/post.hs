{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Twitter.Conduit hiding (map)
import Common

import Control.Applicative
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

main :: IO ()
main = do
    status <- T.concat . map T.pack <$> getArgs
    T.putStrLn $ "Post message: " <> status
    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings
    res <- call twInfo mgr $ update status
    print res
