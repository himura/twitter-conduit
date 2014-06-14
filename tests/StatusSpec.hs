{-# LANGUAGE OverloadedStrings #-}

module StatusSpec where

import Web.Twitter.Conduit (call)
import Web.Twitter.Conduit.Status
import qualified Web.Twitter.Conduit.Parameters as Param
import Web.Twitter.Types.Lens
import Control.Lens

import TestUtils

import Test.Hspec

spec :: Spec
spec = do
    describe "mentionsTimeline" $ do
        it "returns the 20 most resent mentions for user" $ do
            res <- run $ call mentionsTimeline
            res `shouldSatisfy` ((== 20) . length)
        it "returns the most recent mentions of the specified count" $ do
            res <- run . call $ mentionsTimeline & Param.count ?~ 100
            res `shouldSatisfy` ((== 100) . length)

    describe "userTimeline" $ do
        it "returns the 20 most recent tweets posted by the user indicated by ScreenNameParam" $ do
            res <- run . call $ userTimeline (Param.ScreenNameParam "thimura")
            res `shouldSatisfy` ((== 20) . length)
            res `shouldSatisfy` (allOf folded (^. statusUser . userScreenName . to (== "thimura")))
        it "returns the 100 most recent tweets which include RTs when specified include_rts option" $ do
            res <- run . call
                   $ userTimeline (Param.ScreenNameParam "thimura")
                   & Param.count ?~ 100 & Param.includeRts ?~ True
            res `shouldSatisfy` (anyOf (folded . statusRetweet . _Just . statusUser . userScreenName) (/= "thimura"))

