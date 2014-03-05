{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import System.Environment

import Web.Twitter.Conduit
import Common
import qualified Data.Text as T
import Control.Lens

main :: IO ()
main = withCF $ do
    [keyword] <- liftIO getArgs

    res <- call . search $ T.pack keyword
    case res ^. parsed of
        Just pres -> do
            let metadata = searchResultSearchMetadata pres
            liftIO . putStrLn $ "search completed in: " ++ (show . searchMetadataCompletedIn $ metadata)
            liftIO . putStrLn $ "search result max id: " ++ (show . searchMetadataMaxId $ metadata)
            liftIO . print $ searchResultStatuses pres
        Nothing -> return ()
