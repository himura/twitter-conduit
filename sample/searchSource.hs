{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Common

import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import System.Environment

main :: IO ()
main = do
    [num, keyword] <- getArgs

    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings

    res <- sourceWithSearchResult twInfo mgr $ searchTweets $ T.pack keyword
    let metadata = res ^. searchResultSearchMetadata
    putStrLn $ "search completed in: " ++ metadata ^. searchMetadataCompletedIn . to show
    putStrLn $ "search result max id: " ++ metadata ^. searchMetadataMaxId . to show
    runConduit $ res ^. searchResultStatuses .| CL.isolate (read num) .| CL.mapM_ print
