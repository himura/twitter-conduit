{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Web.Twitter.Conduit.Types
       ( Response (..)
       , TwitterError (..)
       , TwitterErrorCode (..)
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
    , responseBody :: responseType
    , responseHeaders :: ResponseHeaders
    } deriving (Show, Eq, Typeable, Functor, Foldable, Traversable)

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
