{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Twitter.Conduit hiding (map)
import Common

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import System.Environment

main :: IO ()
main = do
    [screenName] <- getArgs
    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings
    let sn = ScreenNameParam screenName

    folids <- sourceWithCursor twInfo mgr (followersIds sn) C.$$ CL.consume
    friids <- sourceWithCursor twInfo mgr (friendsIds sn) C.$$ CL.consume

    let folmap = M.fromList $ map (flip (,) True) folids
        os = filter (\uid -> M.notMember uid folmap) friids
        bo = filter (\usr -> M.member usr folmap) friids

    putStrLn "one sided:"
    print os

    putStrLn "both following:"
    print bo
