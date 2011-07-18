{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Status
       ( jsonToStatus
       , jsonToUser
       ) where

import Data.Aeson
import Web.Twitter.Enumerator.Types

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
jsonToStatus val = do
  stId <- lookup "id" val
  stText <- lookup "text" val
  stSource <- lookup "source" val
  stCreated <- lookup "created_at" val
  stTruncated <- lookup "truncated" val
  stUser <- lookup "user" val >>= jsonToUser

  return $ Status
    { statusId = stId
    , statusText = stText
    , statusSource = stSource
    , statusCreated = stCreated
    , statusTruncated = stTruncated
    , statusInReplyTo = lookup "in_reply_to_status_id" val
    , statusInReplyToUser = lookup "in_reply_to_user_id" val
    , statusFavorite = lookup "favrite" val
    , statusUser = stUser
    }

jsonToUser :: Value -> Maybe User
jsonToUser val = do
  uId <- lookup "id" val
  uName <- lookup "name" val
  uScreenName <- lookup "screen_name" val
  uDescription <- lookup "description" val
  uLocation <- lookup "location" val

  return $ User
    { userId = uId
    , userName = uName
    , userScreenName = uScreenName
    , userDescription = uDescription
    , userLocation = uLocation
    , userProfileImageURL = lookup "profile_image_url" val
    , userURL = lookup "url" val
    , userProtected = lookup "protected" val
    , userFollowers = lookup "followers_count" val
    }
