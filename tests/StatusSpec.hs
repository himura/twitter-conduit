{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module StatusSpec where

import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Time
import Network.HTTP.Conduit
import System.IO.Unsafe
import Web.Twitter.Conduit (call, accountVerifyCredentials, sourceWithMaxId, TWInfo)
import qualified Web.Twitter.Conduit.Parameters as Param
import Web.Twitter.Conduit.Status as Status
import Web.Twitter.Types.Lens

import TestUtils
import Test.Hspec

twInfo :: TWInfo
twInfo = unsafePerformIO getTWInfo

mgr :: Manager
mgr = unsafePerformIO $ newManager tlsManagerSettings
{-# NOINLINE mgr #-}

self :: User
self = unsafePerformIO $ call twInfo mgr $ accountVerifyCredentials
{-# NOINLINE self #-}

spec :: Spec
spec = do
    unit
#ifdef RUN_INTEGRATED_TEST
    integrated
#endif

unit :: Spec
unit = return ()

integrated :: Spec
integrated = do
    describe "mentionsTimeline" $ do
        it "returns the 20 most resent mentions for user" $ do
            res <- call twInfo mgr mentionsTimeline
            length res `shouldSatisfy` (> 0)
            let mentionsScreenName = res ^.. traversed . statusEntities . _Just . enUserMentions . traversed . entityBody . userEntityUserScreenName
            mentionsScreenName `shouldSatisfy` allOf folded (== (self ^. userScreenName))
            length mentionsScreenName `shouldSatisfy` (== length res)

    describe "userTimeline" $ do
        it "returns the 20 most recent tweets posted by the user indicated by ScreenNameParam" $ do
            res <- call twInfo mgr $ userTimeline (Param.ScreenNameParam "thimura")
            length res `shouldSatisfy` (== 20)
            res `shouldSatisfy` (allOf folded (^. statusUser . userScreenName . to (== "thimura")))
        it "returns the recent tweets which include RTs when specified include_rts option" $ do
            res <- call twInfo mgr
                   $ userTimeline (Param.ScreenNameParam "thimura")
                   & Param.count ?~ 100 & Param.includeRts ?~ True
            res `shouldSatisfy` (anyOf (folded . statusRetweetedStatus . _Just . statusUser . userScreenName) (/= "thimura"))
        it "iterate with sourceWithMaxId" $ do
            let src = sourceWithMaxId twInfo mgr $
                      userTimeline (Param.ScreenNameParam "thimura") & Param.count ?~ 200
            tl <- src $$ CL.isolate 600 =$ CL.consume
            length tl `shouldSatisfy` (== 600)

            let ids = tl ^.. traversed . statusId
            zip ids (tail ids) `shouldSatisfy` all (\(a, b) -> a > b)

    describe "homeTimeline" $ do
        it "returns the most recent tweets in home timeline" $ do
            res <- call twInfo mgr homeTimeline
            length res `shouldSatisfy` (> 0)

    describe "showId" $ do
        it "works for the known tweets" $ do
            res <- call twInfo mgr $ showId 477833886768959488
            res ^. statusId `shouldBe` 477833886768959488
            res ^. statusText `shouldBe` "真紅かわいいはアレセイア"
            res ^. statusCreatedAt `shouldBe` UTCTime (fromGregorian 2014 6 14) (secondsToDiffTime 55450)
            res ^. statusUser . userScreenName `shouldBe` "thimura"

    describe "update & destroyId" $ do
        it "posts new tweet and destroy it" $ do
            res1 <- call twInfo mgr $ update "おまえの明日が、今日よりもずっと、楽しい事で溢れているようにと、祈っているよ"
            res1 ^. statusUser . userScreenName `shouldBe` self ^. userScreenName

            res2 <- call twInfo mgr $ destroyId (res1 ^. statusId)
            res2 ^. statusId `shouldBe` res1 ^. statusId

    describe "lookup" $ do
        it "works for the known tweets" $ do
            res <- call twInfo mgr $ Status.lookup [438691466345340928, 477757405942411265]
            length res `shouldSatisfy` (== 2)
            (res !! 0) ^. statusId `shouldBe` 438691466345340928
            (res !! 1) ^. statusId `shouldBe` 477757405942411265
