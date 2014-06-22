{-# LANGUAGE OverloadedStrings #-}

module BaseSpec where

import Web.Twitter.Conduit.Base
import Web.Twitter.Conduit.Types
import qualified Web.Twitter.Conduit.Types.Lens as Lens

import Control.Lens
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

import Test.Hspec
import TestUtils

spec :: Spec
spec = do
    unit

unit :: Spec
unit = do
    describe "checkResponse" $ do
        describe "when the response has \"errors\" key" $ do
            let errorMessage = fromJust . decode $ "{\"errors\":[{\"message\":\"Sorry, that page does not exist\",\"code\":34}]}"
                response = Response HT.status404 [] errorMessage
                result = checkResponse response

            it "returns TwitterErrorResponse" $ do
                case result of
                    Left res@(TwitterErrorResponse _ _ msgs) -> do
                        res `shouldBe` TwitterErrorResponse HT.status404 [] [TwitterErrorMessage 34 ""]
                        twitterErrorMessage (head msgs) `shouldBe` "Sorry, that page does not exist"
                    _ -> expectationFailure $ "Unexpected " ++ show result
        describe "when the response does not has \"errors\" key but have error status code" $ do
            let errorMessage = fromJust . decode $ "{}"
                response = Response HT.status404 [] errorMessage
                result = checkResponse response
            it "returns TwitterStatusError" $ do
                case result of
                    Left (TwitterStatusError st hdr body) -> do
                        st `shouldBe` HT.status404
                        hdr `shouldBe` []
                        body `shouldBe` errorMessage
                    _ -> expectationFailure $ "Unexpected " ++ show result
