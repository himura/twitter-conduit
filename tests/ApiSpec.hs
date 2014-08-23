{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module ApiSpec where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Web.Twitter.Conduit (call, sourceWithCursor)
import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Lens
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
    unit
#ifdef RUN_INTEGRATED_TEST
    integrated
#endif

unit :: Spec
unit = return ()

integrated :: Spec
integrated = do
    describe "friendsIds" $ do
        it "returns a cursored collection of users IDs" $ do
            res <- run . call $ friendsIds (Param.ScreenNameParam "thimura")
            res ^. contents . to length `shouldSatisfy` (> 0)

        it "iterate with sourceWithCursor" $ do
            friends <- run $ do
                let src = sourceWithCursor $ friendsIds (Param.ScreenNameParam "thimura")
                src $$ CL.consume
            length friends `shouldSatisfy` (>= 0)

    describe "listsMembers" $ do
        it "returns a cursored collection of the member of specified list" $ do
            res <- run . call $ listsMembers (Param.ListNameParam "thimura/haskell")
            res ^. contents . to length `shouldSatisfy` (>= 0)

        it "should raise error when specified list does not exists" $ do
            let action = run . call $ listsMembers (Param.ListNameParam "thimura/haskell_ne")
            action `shouldThrow` anyException

        it "iterate with sourceWithCursor" $ do
            members <- run $ do
                let src = sourceWithCursor $ listsMembers (Param.ListNameParam "thimura/haskell")
                src $$ CL.consume
            members ^.. traversed . userScreenName `shouldContain` ["Hackage"]
