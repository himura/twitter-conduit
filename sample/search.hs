{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens

import Control.Lens
import qualified Data.Text as T
import System.Environment

main :: IO ()
main = do
    [keyword] <- getArgs

    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings

    res <- call twInfo mgr $ search $ T.pack keyword
    let metadata = res ^. searchResultSearchMetadata
    putStrLn $ "search completed in: " ++ metadata ^. searchMetadataCompletedIn . to show
    putStrLn $ "search result max id: " ++ metadata ^. searchMetadataMaxId . to show
    print $ res ^. searchResultStatuses
