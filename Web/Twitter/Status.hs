{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Status
       ( jsonToStatus
       , jsonToUser
       ) where

import Data.Aeson
import Web.Twitter.Types

import qualified Data.Text as T
import qualified Data.Map as M

import Prelude hiding (lookup)

{-# SPECIALIZE lookup :: T.Text -> Value -> Maybe Value #-}
lookup :: FromJSON a => T.Text -> Value -> Maybe a
lookup key (Object dic) = M.lookup key dic >>= fromJSON'
lookup _ _              = Nothing

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = resultToMaybe . fromJSON

resultToMaybe :: Data.Aeson.Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe _           = Nothing

jsonToStatus :: Value -> Maybe Status
jsonToStatus json = do
  stId <- lookup "id" json
  stText <- lookup "text" json
  stSource <- lookup "source" json
  stCreated <- lookup "created_at" json
  stTruncated <- lookup "truncated" json
  stUser <- lookup "user" json >>= jsonToUser

  return $ Status
    { statusId = stId
    , statusText = stText
    , statusSource = stSource
    , statusCreated = stCreated
    , statusTruncated = stTruncated
    , statusInReplyTo = lookup "in_reply_to_status_id" json
    , statusInReplyToUser = lookup "in_reply_to_user_id" json
    , statusFavorite = lookup "favrite" json
    , statusUser = stUser
    }

jsonToUser :: Value -> Maybe User
jsonToUser json = do
  uId <- lookup "id" json
  uName <- lookup "name" json
  uScreenName <- lookup "screen_name" json
  uDescription <- lookup "description" json
  uLocation <- lookup "location" json

  return $ User
    { userId = uId
    , userName = uName
    , userScreenName = uScreenName
    , userDescription = uDescription
    , userLocation = uLocation
    , userProfileImageURL = lookup "profile_image_url" json
    , userURL = lookup "url" json
    , userProtected = lookup "protected" json
    , userFollowers = lookup "followers_count" json
    }
