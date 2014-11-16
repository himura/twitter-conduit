{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module ApiSpec where

import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit
import System.IO.Unsafe
import TestUtils
import Web.Twitter.Conduit (call, sourceWithCursor, TWInfo)
import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Lens
import qualified Web.Twitter.Conduit.Parameters as Param
import Web.Twitter.Types.Lens

import Test.Hspec

twInfo :: TWInfo
twInfo = unsafePerformIO getTWInfo

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
            res <- withManager $ \mgr -> call twInfo mgr $ friendsIds (Param.ScreenNameParam "thimura")
            res ^. contents . to length `shouldSatisfy` (> 0)

        it "iterate with sourceWithCursor" $ do
            friends <- withManager $ \mgr -> do
                let src = sourceWithCursor twInfo mgr $ friendsIds (Param.ScreenNameParam "thimura")
                src $$ CL.consume
            length friends `shouldSatisfy` (>= 0)

    describe "listsMembers" $ do
        it "returns a cursored collection of the member of specified list" $ do
            res <- withManager $ \mgr -> call twInfo mgr $ listsMembers (Param.ListNameParam "thimura/haskell")
            res ^. contents . to length `shouldSatisfy` (>= 0)

        it "should raise error when specified list does not exists" $ do
            let action = withManager $ \mgr -> call twInfo mgr $ listsMembers (Param.ListNameParam "thimura/haskell_ne")
            action `shouldThrow` anyException

        it "iterate with sourceWithCursor" $ do
            members <- withManager $ \mgr -> do
                let src = sourceWithCursor twInfo mgr $ listsMembers (Param.ListNameParam "thimura/haskell")
                src $$ CL.consume
            members ^.. traversed . userScreenName `shouldContain` ["Hackage"]
