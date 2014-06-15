{-# LANGUAGE OverloadedStrings #-}

module StatusSpec where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Web.Twitter.Conduit (call, accountVerifyCredentials, sourceWithMaxId)
import Web.Twitter.Conduit.Status as Status
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
            length res `shouldSatisfy` (> 0)
            let mentionsScreenName = res ^.. traversed . statusEntities . _Just . enUserMentions . traversed . entityBody . userEntityUserScreenName
            mentionsScreenName `shouldSatisfy` allOf folded (== (self ^. userScreenName))
            length mentionsScreenName `shouldSatisfy` (== length res)

    describe "userTimeline" $ do
        it "returns the 20 most recent tweets posted by the user indicated by ScreenNameParam" $ do
            res <- run . call $ userTimeline (Param.ScreenNameParam "thimura")
            length res `shouldSatisfy` (== 20)
            res `shouldSatisfy` (allOf folded (^. statusUser . userScreenName . to (== "thimura")))
        it "returns the recent tweets which include RTs when specified include_rts option" $ do
            res <- run . call
                   $ userTimeline (Param.ScreenNameParam "thimura")
                   & Param.count ?~ 100 & Param.includeRts ?~ True
            res `shouldSatisfy` (anyOf (folded . statusRetweet . _Just . statusUser . userScreenName) (/= "thimura"))

    describe "homeTimeline" $ do
        it "returns the most recent tweets in home timeline" $ do
            res <- run $ call homeTimeline
            length res `shouldSatisfy` (> 0)

        it "iterate with sourceWithMaxId" $ do
            tl <- run $ do
                let src = sourceWithMaxId $ homeTimeline & Param.count ?~ 200
                src $$ CL.isolate 600 =$ CL.consume
            length tl `shouldSatisfy` (== 600)

            let ids = tl ^.. traversed . statusId
            zip ids (tail ids) `shouldSatisfy` all (\(a, b) -> a > b)

    describe "retweetsOfMe" $ do
        let response = unsafePerformIO . run . call $ retweetsOfMe
        it "returns the most recent tweets authored by the authenticating user" $ do
             let screenNames = response ^.. traversed . statusUser . userScreenName
             screenNames `shouldSatisfy` allOf folded (== (self ^. userScreenName))
             length screenNames `shouldSatisfy` (== length response)
        it "returns the most recent tweets that have been retweeted by other" $ do
            response `shouldSatisfy` allOf (traversed . statusRetweetCount . _Just) (> 0)

    describe "showId" $ do
        it "works for the known tweets" $ do
            res <- run . call $ showId 477833886768959488
            res ^. statusId `shouldBe` 477833886768959488
            res ^. statusText `shouldBe` "真紅かわいいはアレセイア"
            res ^. statusCreatedAt `shouldBe` "Sat Jun 14 15:24:10 +0000 2014"
            res ^. statusUser . userScreenName `shouldBe` "thimura"

    describe "update & destroyId" $ do
        it "posts new tweet and destroy it" $ do
            res1 <- run . call $ update "おまえの明日が、今日よりもずっと、楽しい事で溢れているようにと、祈っているよ"
            res1 ^. statusUser . userScreenName `shouldBe` self ^. userScreenName

            res2 <- run . call $ destroyId (res1 ^. statusId)
            res2 ^. statusId `shouldBe` res1 ^. statusId

    describe "lookup" $ do
        it "works for the known tweets" $ do
            res <- run . call $ Status.lookup [438691466345340928, 477757405942411265]
            length res `shouldSatisfy` (== 2)
            (res !! 0) ^. statusId `shouldBe` 438691466345340928
            (res !! 1) ^. statusId `shouldBe` 477757405942411265
