{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Conduit.Error
       ( TwitterError (..)
       , TwitterErrorCode (..)
       ) where

import Control.Applicative
import Control.Exception
import Data.Aeson
import Data.Data
import qualified Data.Text as T
import Network.HTTP.Types (Status, ResponseHeaders)

data TwitterError
    = ParseError String
    | TwitterStatusError Status ResponseHeaders TwitterErrorCode
    deriving (Show, Typeable)

instance Exception TwitterError

-- | Twitter Error Codes
--
-- see detail: <https://dev.twitter.com/docs/error-codes-responses>
data TwitterErrorCode = TwitterErrorCode
    { twitterErrorCode :: Int
    , twitterErrorMessage :: T.Text
    } deriving (Show, Data, Typeable)

instance Eq TwitterErrorCode where
    TwitterErrorCode { twitterErrorCode = a } == TwitterErrorCode { twitterErrorCode = b }
        = a == b

instance Ord TwitterErrorCode where
    compare TwitterErrorCode { twitterErrorCode = a } TwitterErrorCode { twitterErrorCode = b }
        = a `compare` b

instance Enum TwitterErrorCode where
    fromEnum = twitterErrorCode
    toEnum a = TwitterErrorCode a T.empty

instance FromJSON TwitterErrorCode where
    parseJSON (Object o) =
        TwitterErrorCode
        <$> o .:  "code"
        <*> o .:  "message"
    parseJSON v = fail $ "unexpected: " ++ show v
