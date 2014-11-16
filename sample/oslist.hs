{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Twitter.Conduit hiding (map)
import Common

import Control.Monad.IO.Class
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import Network.HTTP.Conduit
import System.Environment

main :: IO ()
main = do
    [screenName] <- liftIO getArgs
    twInfo <- getTWInfoFromEnv
    let sn = ScreenNameParam screenName

    (folids, friids) <- withManager $ \mgr -> do
        folids <- sourceWithCursor twInfo mgr (followersIds sn) C.$$ CL.consume
        friids <- sourceWithCursor twInfo mgr (friendsIds sn) C.$$ CL.consume
        return (folids, friids)

    let folmap = M.fromList $ map (flip (,) True) folids
        os = filter (\uid -> M.notMember uid folmap) friids
        bo = filter (\usr -> M.member usr folmap) friids

    liftIO $ putStrLn "one sided:"
    liftIO $ print os

    liftIO $ putStrLn "both following:"
    liftIO $ print bo
