{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import System.Environment

import Web.Twitter.Conduit
import Common
import qualified Data.Text as T
import Control.Lens

main :: IO ()
main = runTwitterFromEnv' $ do
    [keyword] <- liftIO getArgs

    res <- call . search $ T.pack keyword
    let metadata = res ^. searchResultSearchMetadata
    liftIO . putStrLn $ "search completed in: " ++ metadata ^. searchMetadataCompletedIn . to show
    liftIO . putStrLn $ "search result max id: " ++ metadata ^. searchMetadataMaxId . to show
    liftIO . print $ res ^. searchResultStatuses
