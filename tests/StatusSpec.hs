{-# LANGUAGE OverloadedStrings #-}

module StatusSpec where

import Web.Twitter.Conduit (call, accountVerifyCredentials)
import Web.Twitter.Conduit.Status
import qualified Web.Twitter.Conduit.Parameters as Param
import Web.Twitter.Types.Lens
import Control.Lens
import TestUtils

import Test.Hspec

import System.IO.Unsafe

self :: User
self = unsafePerformIO . run . call $ accountVerifyCredentials

spec :: Spec
spec = do
    describe "mentionsTimeline" $ do
        it "returns the 20 most resent mentions for user" $ do
            res <- run $ call mentionsTimeline
            res `shouldSatisfy` ((> 0) . length)
            let mentionsScreenName = res ^.. traversed . statusEntities . _Just . enUserMentions . traversed . entityBody . userEntityUserScreenName
            mentionsScreenName `shouldSatisfy` allOf folded (== (self ^. userScreenName))
            mentionsScreenName `shouldSatisfy` ((== length res) . length)

    describe "userTimeline" $ do
        it "returns the 20 most recent tweets posted by the user indicated by ScreenNameParam" $ do
            res <- run . call $ userTimeline (Param.ScreenNameParam "thimura")
            res `shouldSatisfy` ((== 20) . length)
            res `shouldSatisfy` (allOf folded (^. statusUser . userScreenName . to (== "thimura")))
        it "returns the recent tweets which include RTs when specified include_rts option" $ do
            res <- run . call
                   $ userTimeline (Param.ScreenNameParam "thimura")
                   & Param.count ?~ 100 & Param.includeRts ?~ True
            res `shouldSatisfy` (anyOf (folded . statusRetweet . _Just . statusUser . userScreenName) (/= "thimura"))

    describe "homeTimeline" $ do
        it "returns the most recent tweets in home timeline" $ do
            res <- run $ call homeTimeline
            res `shouldSatisfy` ((> 0) . length)

    describe "retweetsOfMe" $ do
        let response = unsafePerformIO . run . call $ retweetsOfMe
        it "returns the most recent tweets authored by the authenticating user" $ do
             let screenNames = response ^.. traversed . statusUser . userScreenName
             screenNames `shouldSatisfy` allOf folded (== (self ^. userScreenName))
             screenNames `shouldSatisfy` ((== length response) . length)
        it "returns the most recent tweets that have been retweeted by other" $ do
            response `shouldSatisfy` allOf (traversed . statusRetweetCount . _Just) (> 0)
