{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Web.Twitter.Conduit.Types
       ( Response (..)
       , TwitterError (..)
       , TwitterErrorMessage (..)
       , MediaData (..)
       , UploadedMedia (..)
       , ImageSizeType (..)
       ) where

import Control.Applicative
import Control.Exception
import Data.Aeson
import Data.Data
import Data.Foldable (Foldable)
import qualified Data.Text as T
import Data.Traversable (Traversable)
import Network.HTTP.Client (RequestBody)
import Network.HTTP.Types (Status, ResponseHeaders)

data MediaData = MediaFromFile FilePath
               | MediaRequestBody FilePath RequestBody

data ImageSizeType = ImageSizeType
    { imageWidth :: Int
    , imageHeight :: Int
    , imageType :: T.Text
    } deriving Show
instance FromJSON ImageSizeType where
    parseJSON (Object o) =
        ImageSizeType <$> o .:  "w"
                      <*> o .:  "h"
                      <*> o .:  "image_type"
    parseJSON v = fail $ "unknown value: " ++ show v

data UploadedMedia = UploadedMedia
    { mediaId :: Integer
    , mediaSize :: Integer
    , mediaImage :: ImageSizeType
    } deriving Show
instance FromJSON UploadedMedia where
    parseJSON (Object o) =
        UploadedMedia <$> o .:  "media_id"
                      <*> o .:  "size"
                      <*> o .:  "image"
    parseJSON v = fail $ "unknown value: " ++ show v

data Response responseType = Response
    { responseStatus :: Status
    , responseHeaders :: ResponseHeaders
    , responseBody :: responseType
    } deriving (Show, Eq, Typeable, Functor, Foldable, Traversable)

data TwitterError
    = FromJSONError String
    | TwitterErrorResponse Status ResponseHeaders [TwitterErrorMessage]
    | TwitterStatusError Status ResponseHeaders Value
    deriving (Show, Typeable, Eq)

instance Exception TwitterError

-- | Twitter Error Messages
--
-- see detail: <https://dev.twitter.com/docs/error-codes-responses>
data TwitterErrorMessage = TwitterErrorMessage
    { twitterErrorCode :: Int
    , twitterErrorMessage :: T.Text
    } deriving (Show, Data, Typeable)

instance Eq TwitterErrorMessage where
    TwitterErrorMessage { twitterErrorCode = a } == TwitterErrorMessage { twitterErrorCode = b }
        = a == b

instance Ord TwitterErrorMessage where
    compare TwitterErrorMessage { twitterErrorCode = a } TwitterErrorMessage { twitterErrorCode = b }
        = a `compare` b

instance Enum TwitterErrorMessage where
    fromEnum = twitterErrorCode
    toEnum a = TwitterErrorMessage a T.empty

instance FromJSON TwitterErrorMessage where
    parseJSON (Object o) =
        TwitterErrorMessage
        <$> o .:  "code"
        <*> o .:  "message"
    parseJSON v = fail $ "unexpected: " ++ show v
