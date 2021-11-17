{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiSpec where

import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Time
import Network.HTTP.Conduit
import System.IO.Unsafe
import Test.Hspec
import TestUtils
import Web.Twitter.Conduit (TWInfo, accountVerifyCredentials, call, sourceWithCursor, sourceWithMaxId)
import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Lens
import qualified Web.Twitter.Conduit.Parameters as Param
import Web.Twitter.Types.Lens

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
        it "returns the 20 most recent mentions for user" $ do
            res <- call twInfo mgr statusesMentionsTimeline
            length res `shouldSatisfy` (> 0)
            let mentionsScreenName = res ^.. traversed . statusEntities . _Just . enUserMentions . traversed . entityBody . userEntityUserScreenName
            mentionsScreenName `shouldSatisfy` allOf folded (== (self ^. userScreenName))
            length mentionsScreenName `shouldSatisfy` (== length res)

    describe "userTimeline" $ do
        it "returns the 20 most recent tweets posted by the user indicated by ScreenNameParam" $ do
            res <- call twInfo mgr $ statusesUserTimeline (Param.ScreenNameParam "thimura")
            length res `shouldSatisfy` (== 20)
            res `shouldSatisfy` (allOf folded (^. statusUser . userScreenName . to (== "thimura")))
        it "returns the recent tweets which include RTs when specified include_rts option" $ do
            res <-
                call twInfo mgr $
                    statusesUserTimeline (Param.ScreenNameParam "thimura")
                        & #count ?~ 100
                        & #include_rts ?~ True
            res `shouldSatisfy` (anyOf (folded . statusRetweetedStatus . _Just . statusUser . userScreenName) (/= "thimura"))
        it "iterate with sourceWithMaxId" $ do
            let src =
                    sourceWithMaxId twInfo mgr $
                        statusesUserTimeline (Param.ScreenNameParam "thimura") & #count ?~ 200
            tl <- runConduit $ src .| CL.isolate 600 .| CL.consume
            length tl `shouldSatisfy` (== 600)

            let ids = tl ^.. traversed . statusId
            zip ids (tail ids) `shouldSatisfy` all (\(a, b) -> a > b)

    describe "homeTimeline" $ do
        it "returns the most recent tweets in home timeline" $ do
            res <- call twInfo mgr statusesHomeTimeline
            length res `shouldSatisfy` (> 0)

    describe "showId" $ do
        it "works for the known tweets" $ do
            res <- call twInfo mgr $ statusesShowId 477833886768959488
            res ^. statusId `shouldBe` 477833886768959488
            res ^. statusText `shouldBe` "真紅かわいいはアレセイア"
            res ^. statusCreatedAt `shouldBe` UTCTime (fromGregorian 2014 6 14) (secondsToDiffTime 55450)
            res ^. statusUser . userScreenName `shouldBe` "thimura"

    describe "update & destroyId" $ do
        it "posts new tweet and destroy it" $ do
            res1 <- call twInfo mgr $ statusesUpdate "おまえの明日が、今日よりもずっと、楽しい事で溢れているようにと、祈っているよ"
            res1 ^. statusUser . userScreenName `shouldBe` self ^. userScreenName

            res2 <- call twInfo mgr $ statusesDestroyId (res1 ^. statusId)
            res2 ^. statusId `shouldBe` res1 ^. statusId

    describe "lookup" $ do
        it "works for the known tweets" $ do
            res <- call twInfo mgr $ statusesLookup [438691466345340928, 477757405942411265]
            length res `shouldSatisfy` (== 2)
            (res !! 0) ^. statusId `shouldBe` 438691466345340928
            (res !! 1) ^. statusId `shouldBe` 477757405942411265

        it "handles extended tweets" $ do
            res <- call twInfo mgr $ statusesLookup [1128358947772145672]
            (res !! 0) ^. statusText `shouldBe` "Through the Twitter Developer Labs program, we'll soon preview new versions of GET /tweets and GET /users, followed\8230 https://t.co/9i4c5bUUCu"
            res <- call twInfo mgr $ statusesLookup [1128358947772145672] & #tweet_mode ?~ Param.Extended
            (res !! 0) ^. statusText `shouldBe` "Through the Twitter Developer Labs program, we'll soon preview new versions of GET /tweets and GET /users, followed by Tweet streaming, search &amp; metrics. More to come! \128073 https://t.co/rDE48yNiSw https://t.co/oFsvkpnDhS"

    describe "friendsIds" $ do
        it "returns a cursored collection of users IDs" $ do
            res <- call twInfo mgr $ friendsIds (Param.ScreenNameParam "thimura")
            res ^. contents . to length `shouldSatisfy` (> 0)

        it "iterate with sourceWithCursor" $ do
            let src = sourceWithCursor twInfo mgr $ friendsIds (Param.ScreenNameParam "thimura")
            friends <- runConduit $ src .| CL.consume
            length friends `shouldSatisfy` (>= 0)

    describe "listsMembers" $ do
        it "returns a cursored collection of the member of specified list" $ do
            res <- call twInfo mgr $ listsMembers (Param.ListNameParam "thimura/haskell")
            res ^. contents . to length `shouldSatisfy` (>= 0)

        it "should raise error when specified list does not exists" $ do
            let action = call twInfo mgr $ listsMembers (Param.ListNameParam "thimura/haskell_ne")
            action `shouldThrow` anyException

        it "iterate with sourceWithCursor" $ do
            let src = sourceWithCursor twInfo mgr $ listsMembers (Param.ListNameParam "thimura/haskell")
            members <- runConduit $ src .| CL.consume
            members ^.. traversed . userScreenName `shouldContain` ["Hackage"]
