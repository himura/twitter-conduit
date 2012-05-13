{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Web.Twitter.Conduit.Types
       ( TwitterException(..)
       , DateString
       , UserId
       , URLString
       , UserName
       , StatusId
       , StreamingAPI(..)
       , Status(..)
       , RetweetedStatus(..)
       , EventTarget(..)
       , Event(..)
       , Delete(..)
       , User(..)
       , List(..)
       , checkError
       )
       where

import qualified Network.HTTP.Types as HT
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text as T
import Data.ByteString (ByteString)
import Data.Typeable
import Control.Applicative
import Control.Monad
import Control.Exception

data TwitterException = HTTPStatusCodeException HT.Status
                      | ParserException SomeException [ByteString]
                      | TwitterErrorMessage T.Text Value
                      deriving (Show, Typeable)
instance Exception TwitterException

type DateString  = String
type UserId      = Integer
type URLString   = String
type UserName    = T.Text
type StatusId    = Integer

data StreamingAPI = SStatus Status
                  | SRetweetedStatus RetweetedStatus
                  | SEvent Event
                  | SDelete Delete
                  -- -- | SScrubGeo ScrubGeo
                  | SFriends Friends
                  | SUnknown Value
                  deriving (Show, Eq)

checkError :: Object -> Parser a
checkError o = do
  err <- o .:? "error"
  case err of
    Just msg -> throw $ TwitterErrorMessage msg (Object o)
    Nothing -> mzero

instance FromJSON StreamingAPI where
  parseJSON v@(Object _) =
    SRetweetedStatus <$> js <|>
    SStatus <$> js <|>
    SEvent <$> js <|>
    SDelete <$> js <|>
    SFriends <$> js <|>
    (return $ SUnknown v)
    where
      js :: FromJSON a => Parser a
      js = parseJSON v
  parseJSON _ = mzero

data Status =
  Status
  { statusCreatedAt     :: DateString
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
  parseJSON (Object o) = checkError o <|>
    Status <$> o .:  "created_at"
           <*> o .:  "id"
           <*> o .:  "text"
           <*> o .:  "source"
           <*> o .:  "truncated"
           <*> o .:? "in_reply_to_status_id"
           <*> o .:? "in_reply_to_user_id"
           <*> o .:? "favorite"
           <*> o .:  "user"
  parseJSON _ = mzero

data RetweetedStatus =
  RetweetedStatus
  { rsCreatedAt       :: DateString
  , rsId              :: StatusId
  , rsText            :: T.Text
  , rsSource          :: String
  , rsTruncated       :: Bool
  , rsUser            :: User
  , rsRetweetedStatus :: Status
  } deriving (Show, Eq)

instance FromJSON RetweetedStatus where
  parseJSON (Object o) = checkError o <|>
    RetweetedStatus <$> o .: "created_at"
                    <*> o .: "id"
                    <*> o .: "text"
                    <*> o .: "source"
                    <*> o .: "truncated"
                    <*> o .: "user"
                    <*> o .: "retweeted_status"
  parseJSON _ = mzero

data EventType = Favorite | Unfavorite
               | ListCreated | ListUpdated | ListMemberAdded
               | UserUpdate | Block | Unblock | Follow
               deriving (Show, Eq)

data EventTarget = ETUser User | ETStatus Status | ETList List | ETUnknown Value
                 deriving (Show, Eq)

instance FromJSON EventTarget where
  parseJSON v@(Object o) = checkError o <|>
    ETUser <$> parseJSON v <|>
    ETStatus <$> parseJSON v <|>
    ETList <$> parseJSON v <|>
    (return $ ETUnknown v)
  parseJSON _ = mzero

data Event =
  Event
  { evCreatedAt       :: DateString
  , evTargetObject    :: Maybe EventTarget
  , evEvent           :: String
  , evTarget          :: EventTarget
  , evSource          :: EventTarget
  } deriving (Show, Eq)

instance FromJSON Event where
  parseJSON (Object o) = checkError o <|>
    Event <$> o .:  "created_at"
          <*> o .:? "target_object"
          <*> o .:  "event"
          <*> o .:  "target"
          <*> o .:  "source"
  parseJSON _ = mzero

data Delete =
  Delete
  { delId  :: StatusId
  , delUserId :: UserId
  } deriving (Show, Eq)

instance FromJSON Delete where
  parseJSON (Object o) = checkError o <|> do
    s <- o .: "delete" >>= (.: "status")
    Delete <$> s .: "id"
           <*> s .: "user_id"
  parseJSON _ = mzero

type Friends = [UserId]

data User =
  User
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
  parseJSON (Object o) = checkError o <|>
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

data List =
  List
  { listId :: Int
  , listName :: String
  , listFullName :: String
  , listMemberCount :: Int
  , listSubscriberCount :: Int
  , listMode :: String
  , listUser :: User
  } deriving (Show, Eq)

instance FromJSON List where
  parseJSON (Object o) = checkError o <|>
    List <$> o .: "id"
         <*> o .: "name"
         <*> o .: "full_name"
         <*> o .: "member_count"
         <*> o .: "subscriber_count"
         <*> o .: "mode"
         <*> o .: "user"
  parseJSON _ = mzero
