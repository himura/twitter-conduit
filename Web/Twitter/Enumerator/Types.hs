{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Types
       ( DateString
       , UserId
       , URLString
       , UserName
       , StatusId
       , Status(..)
       , User(..)
       )
       where

import Data.Aeson
import Data.Text as T
import Control.Applicative
import Control.Monad

type DateString  = String
type UserId      = Integer
type URLString   = String
type UserName    = T.Text
type StatusId    = Integer

data Status
 = Status
     { statusCreated       :: DateString
     , statusId            :: StatusId
     , statusText          :: T.Text
     , statusSource        :: String
     , statusTruncated     :: Bool
     , statusInReplyTo     :: Maybe StatusId
     , statusInReplyToUser :: Maybe UserId
     , statusFavorite      :: Maybe Bool
     , statusUser          :: User
     } deriving (Show, Eq)

instance FromJSON Status where
  parseJSON (Object o) =
    Status <$> o .:  "created_at"
           <*> o .:  "id"
           <*> o .:  "text"
           <*> o .:  "source"
           <*> o .:  "truncated"
           <*> o .:? "in_reply_to_status_id"
           <*> o .:? "in_reply_to_user_id"
           <*> o .:? "favorite"
           <*> (o .: "user" >>= parseJSON)
  parseJSON _ = mzero

data User
  = User
     { userId              :: UserId
     , userName            :: UserName
     , userScreenName      :: String
     , userDescription     :: T.Text
     , userLocation        :: T.Text
     , userProfileImageURL :: Maybe URLString
     , userURL             :: Maybe URLString
     , userProtected       :: Maybe Bool
     , userFollowers       :: Maybe Int
     } deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Object o) =
    User <$> o .:  "id"
         <*> o .:  "name"
         <*> o .:  "screen_name"
         <*> o .:  "description"
         <*> o .:  "location"
         <*> o .:? "profile_image_url"
         <*> o .:? "url"
         <*> o .:? "protected"
         <*> o .:? "followers_count"
  parseJSON _ = mzero
