{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Twitter.Conduit.Types
       ( MediaData (..)
       , UploadedMedia (..)
       , mediaId
       , mediaSize
       , mediaImage
       , ImageSizeType (..)
       , imageWidth
       , imageHeight
       , imageType
       ) where

import Network.HTTP.Client (RequestBody)
import Data.Aeson
import Data.Text (Text)
import Control.Lens
import Control.Applicative

data MediaData = MediaFromFile FilePath
               | MediaRequestBody FilePath RequestBody

data ImageSizeType = ImageSizeType
    { _imageWidth :: Int
    , _imageHeight :: Int
    , _imageType :: Text
    } deriving Show
makeLenses ''ImageSizeType
instance FromJSON ImageSizeType where
    parseJSON (Object o) =
        ImageSizeType <$> o .:  "w"
                      <*> o .:  "h"
                      <*> o .:  "image_type"
    parseJSON v = fail $ "unknown value: " ++ show v

data UploadedMedia = UploadedMedia
    { _mediaId :: Integer
    , _mediaSize :: Integer
    , _mediaImage :: ImageSizeType
    } deriving Show
makeLenses ''UploadedMedia
instance FromJSON UploadedMedia where
    parseJSON (Object o) =
        UploadedMedia <$> o .:  "media_id"
                      <*> o .:  "size"
                      <*> o .:  "image"
    parseJSON v = fail $ "unknown value: " ++ show v
