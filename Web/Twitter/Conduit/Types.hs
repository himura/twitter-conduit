{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Web.Twitter.Conduit.Types
       (
#if !MIN_VERSION_twitter_types(0,5,0)
       , UploadedMedia (..)
       , ImageSizeType (..)
#endif
       ) where

import Control.Applicative
import Data.Aeson
import qualified Data.Text as T

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
