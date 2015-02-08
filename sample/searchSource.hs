{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Common

import Control.Lens
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Network.HTTP.Conduit
import System.Environment

main :: IO ()
main = do
    [num, keyword] <- getArgs

    twInfo <- getTWInfoFromEnv

    withManager $ \mgr -> do
        res <- sourceWithSearchResult twInfo mgr $ searchTweets $ T.pack keyword
        let metadata = res ^. searchResultSearchMetadata
        liftIO . putStrLn $ "search completed in: " ++ metadata ^. searchMetadataCompletedIn . to show
        liftIO . putStrLn $ "search result max id: " ++ metadata ^. searchMetadataMaxId . to show
        res ^. searchResultStatuses $$ CL.isolate (read num) =$ CL.mapM_ (liftIO . print)
