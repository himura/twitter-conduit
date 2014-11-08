{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Common

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Text as T
import Network.HTTP.Conduit
import System.Environment

main :: IO ()
main = do
    [keyword] <- getArgs

    twInfo <- getTWInfoFromEnv

    res <- withManager $ \mgr -> call twInfo mgr $ search $ T.pack keyword
    let metadata = res ^. searchResultSearchMetadata
    liftIO . putStrLn $ "search completed in: " ++ metadata ^. searchMetadataCompletedIn . to show
    liftIO . putStrLn $ "search result max id: " ++ metadata ^. searchMetadataMaxId . to show
    liftIO . print $ res ^. searchResultStatuses
