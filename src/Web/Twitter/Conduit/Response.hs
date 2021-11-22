{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Conduit.Response (
    Response (..),
    TwitterError (..),
    TwitterErrorMessage (..),
) where

import Control.Exception
import Data.Aeson
import Data.Data
import qualified Data.Text as T
import Network.HTTP.Types (ResponseHeaders, Status)

data Response responseType = Response
    { responseStatus :: Status
    , responseHeaders :: ResponseHeaders
    , responseBody :: responseType
    }
    deriving (Show, Eq, Typeable, Functor, Foldable, Traversable)

data TwitterError
    = FromJSONError String
    | TwitterErrorResponse Status ResponseHeaders [TwitterErrorMessage]
    | TwitterUnknownErrorResponse Status ResponseHeaders Value
    | TwitterStatusError Status ResponseHeaders Value
    deriving (Show, Typeable, Eq)

instance Exception TwitterError

-- | Twitter Error Messages
--
-- see detail: <https://dev.twitter.com/docs/error-codes-responses>
data TwitterErrorMessage = TwitterErrorMessage
    { twitterErrorCode :: Int
    , twitterErrorMessage :: T.Text
    }
    deriving (Show, Eq, Ord, Data, Typeable)

instance Enum TwitterErrorMessage where
    fromEnum = twitterErrorCode
    toEnum a = TwitterErrorMessage a T.empty

instance FromJSON TwitterErrorMessage where
    parseJSON (Object o) =
        TwitterErrorMessage
            <$> o .: "code"
            <*> o .: "message"
    parseJSON v = fail $ "unexpected: " ++ show v
