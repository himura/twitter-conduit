{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Twitter.Conduit
import Common

import Control.Lens
import System.Environment

main :: IO ()
main = do
    [statusIdStr] <- getArgs
    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings
    let sId = read statusIdStr

    targetStatus <- call twInfo mgr $ statusesShowId sId
    putStrLn $ "Favorite Tweet: " ++ targetStatus ^. to show
    res <- call twInfo mgr $ favoritesCreate sId
    print res
