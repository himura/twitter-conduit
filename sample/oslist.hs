{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import Control.Monad.IO.Class
import System.Environment

import Web.Twitter.Conduit
import Common

main :: IO ()
main = runTwitterFromEnv' $ do
    [screenName] <- liftIO getArgs
    let sn = ScreenNameParam screenName
    folids <- sourceWithCursor (followersIds sn) C.$$ CL.consume
    friids <- sourceWithCursor (friendsIds sn) C.$$ CL.consume

    let folmap = M.fromList $ map (flip (,) True) folids
        os = filter (\uid -> M.notMember uid folmap) friids
        bo = filter (\usr -> M.member usr folmap) friids

    liftIO $ putStrLn "one sided:"
    liftIO $ print os

    liftIO $ putStrLn "both following:"
    liftIO $ print bo
