{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Twitter.Conduit
import Common

import Control.Lens
import Control.Monad.IO.Class
import Network.HTTP.Conduit
import System.Environment

main :: IO ()
main = do
    [statusIdStr] <- liftIO getArgs
    twInfo <- getTWInfoFromEnv
    let sId = read statusIdStr
    withManager $ \mgr -> do
        targetStatus <- call twInfo mgr $ showId sId
        liftIO . putStrLn $ "Favorite Tweet: " ++ targetStatus ^. to show
        res <- call twInfo mgr $ favoritesCreate sId
        liftIO $ print res
