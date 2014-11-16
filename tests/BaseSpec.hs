{-# LANGUAGE OverloadedStrings #-}

module BaseSpec where

import Web.Twitter.Conduit.Response
import Web.Twitter.Conduit.Base

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import Data.Maybe
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

import Test.Hspec

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

    describe "sinkJSON" $ do
        describe "when valid JSON input" $ do
            let input = "{\"test\": \"input\", \"status\": 200 }"
            it "can consume the input from Source and returns JSON Value" $ do
                res <- yield input $$ sinkJSON
                res ^. key "test" . _String `shouldBe` "input"
                res ^? key "status" . _Integer `shouldBe` Just 200
        describe "when invalid JSON input" $ do
            let input = "{]"
            it "should raise Data.Conduit.Attoparsec.ParseError" $ do
                let parseErrorException (CA.ParseError {}) = True
                    parseErrorException _ = False
                    action = yield input $$ sinkJSON
                action `shouldThrow` parseErrorException

    describe "sinkFromJSON" $ do
        describe "when valid JSON input" $ do
            let input = "{\"test\": \"input\", \"status\": 200 }"
            it "can consume the input from Source and returns a value which type is the specified one" $ do
                res <- yield input $$ sinkFromJSON
                res `shouldBe` TestJSON "input" 200
        describe "when the JSON value does not have expected format" $ do
            let input = "{\"status\": 200}"
            it "should raise FromJSONError" $ do
                let fromJSONException (FromJSONError {}) = True
                    fromJSONException _ = False
                    action :: IO TestJSON
                    action = yield input $$ sinkFromJSON
                action `shouldThrow` fromJSONException

data TestJSON = TestJSON
    { testField :: T.Text
    , testStatus :: Int
    } deriving (Show, Eq)
instance FromJSON TestJSON where
    parseJSON (Object o) =
        TestJSON <$> o .: "test"
                 <*> o .: "status"
    parseJSON v = fail $ "Unexpected: " ++ show v
