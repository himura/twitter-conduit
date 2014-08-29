{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Web.Twitter.Conduit.Types
       ( Response (..)
       , TwitterError (..)
       , TwitterErrorMessage (..)
       , CursorKey (..)
       , IdsCursorKey
       , UsersCursorKey
       , ListsCursorKey
       , WithCursor (..)
       , MediaData (..)
#if !MIN_VERSION_twitter_types(0,5,0)
       , UploadedMedia (..)
       , ImageSizeType (..)
#endif
       ) where


import Control.Applicative
import Control.Exception
import Data.Aeson
import Data.Data
import Data.Foldable (Foldable)
import Data.Monoid
import qualified Data.Text as T
import Data.Traversable (Traversable)
import Network.HTTP.Client (RequestBody)
import Network.HTTP.Types (Status, ResponseHeaders)
import Web.Twitter.Types (checkError)

-- $setup
-- >>> type UserId = Integer


class CursorKey a where
    cursorKey :: a -> T.Text

-- | Phantom type to specify the key which point out the content in the response.
data IdsCursorKey
instance CursorKey IdsCursorKey where
    cursorKey = const "ids"

-- | Phantom type to specify the key which point out the content in the response.
data UsersCursorKey
instance CursorKey UsersCursorKey where
    cursorKey = const "users"

-- | Phantom type to specify the key which point out the content in the response.
data ListsCursorKey
instance CursorKey ListsCursorKey where
    cursorKey = const "lists"

#if __GLASGOW_HASKELL__ >= 706
-- | A wrapper for API responses which have "next_cursor" field.
--
-- The first type parameter of 'WithCursor' specifies the field name of contents.
--
-- >>> let Just res = decode "{\"previous_cursor\": 0, \"next_cursor\": 1234567890, \"ids\": [1111111111]}" :: Maybe (WithCursor IdsCursorKey UserId)
-- >>> nextCursor res
-- 1234567890
-- >>> contents res
-- [1111111111]
--
-- >>> let Just res = decode "{\"previous_cursor\": 0, \"next_cursor\": 0, \"users\": [1000]}" :: Maybe (WithCursor UsersCursorKey UserId)
-- >>> nextCursor res
-- 0
-- >>> contents res
-- [1000]
#endif
data WithCursor cursorKey wrapped = WithCursor
    { previousCursor :: Integer
    , nextCursor :: Integer
    , contents :: [wrapped]
    } deriving Show

instance (FromJSON wrapped, CursorKey c) =>
         FromJSON (WithCursor c wrapped) where
    parseJSON (Object o) = checkError o >>
      WithCursor <$> o .:  "previous_cursor"
                 <*> o .:  "next_cursor"
                 <*> o .:  cursorKey (undefined :: c)
    parseJSON _ = mempty

data MediaData = MediaFromFile FilePath
               | MediaRequestBody FilePath RequestBody

#if !MIN_VERSION_twitter_types(0,5,0)
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
#endif

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
