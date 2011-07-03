{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Types where

import Data.Text as T

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

nullStatus :: Status
nullStatus = Status
     { statusCreated       = ""
     , statusId            = 0
     , statusText          = ""
     , statusSource        = ""
     , statusTruncated     = False
     , statusInReplyTo     = Nothing
     , statusInReplyToUser = Nothing
     , statusFavorite      = Nothing
     , statusUser          = nullUser
     }

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

nullUser :: User
nullUser = User
     { userId              = 0
     , userName            = ""
     , userScreenName      = ""
     , userDescription     = ""
     , userLocation        = ""
     , userProfileImageURL = Nothing
     , userURL             = Nothing
     , userProtected       = Nothing
     , userFollowers       = Nothing
     }
