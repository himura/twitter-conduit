{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Web.Twitter.Enumerator.Types
       ( TwitterException(..)
       , DateString
       , UserId
       , Friends
       , URLString
       , UserName
       , StatusId
       , LanguageCode
       , StreamingAPI(..)
       , Status(..)
       , SearchStatus(..)
       , RetweetedStatus(..)
       , EventTarget(..)
       , Event(..)
       , Delete(..)
       , User(..)
       , List(..)
       , Entities(..)
       , Entity(..)
       , HashTagEntity(..)
       , UserEntity(..)
       , URLEntity(..)
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
                      | MissingCredential
                      deriving (Show, Typeable)
instance Exception TwitterException

type DateString   = String
type UserId       = Integer
type Friends      = [UserId]
type URLString    = String
type UserName     = T.Text
type StatusId     = Integer
type LanguageCode = String

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
    return (SUnknown v)
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
  , statusEntities      :: Maybe Entities
  , statusInReplyTo     :: Maybe StatusId
  , statusInReplyToUser :: Maybe UserId
  , statusFavorite      :: Maybe Bool
  , statusRetweetCount  :: Maybe Integer
  , statusUser          :: User
  } deriving (Show, Eq)

instance FromJSON Status where
  parseJSON (Object o) = checkError o <|>
    Status <$> o .:  "created_at"
           <*> o .:  "id"
           <*> o .:  "text"
           <*> o .:  "source"
           <*> o .:  "truncated"
           <*> o .:? "entities"
           <*> o .:? "in_reply_to_status_id"
           <*> o .:? "in_reply_to_user_id"
           <*> o .:? "favorited"
           <*> o .:? "retweet_count"
           <*> o .:  "user"
  parseJSON _ = mzero

data SearchStatus =
  SearchStatus
  { searchStatusCreatedAt     :: DateString
  , searchStatusId            :: StatusId
  , searchStatusText          :: T.Text
  , searchStatusSource        :: String
  , searchStatusUserId        :: UserId
  , searchStatusUserName      :: UserName
  } deriving (Show, Eq)

instance FromJSON SearchStatus where
  parseJSON (Object o) = checkError o <|>
    SearchStatus <$> o .:  "created_at"
                 <*> o .:  "id"
                 <*> o .:  "text"
                 <*> o .:  "source"
                 <*> o .:  "from_user_id"
                 <*> o .:  "from_user"
  parseJSON _ = mzero

data RetweetedStatus =
  RetweetedStatus
  { rsCreatedAt       :: DateString
  , rsId              :: StatusId
  , rsText            :: T.Text
  , rsSource          :: String
  , rsTruncated       :: Bool
  , rsEntities        :: Maybe Entities
  , rsUser            :: User
  , rsRetweetedStatus :: Status
  } deriving (Show, Eq)

instance FromJSON RetweetedStatus where
  parseJSON (Object o) = checkError o <|>
    RetweetedStatus <$> o .:  "created_at"
                    <*> o .:  "id"
                    <*> o .:  "text"
                    <*> o .:  "source"
                    <*> o .:  "truncated"
                    <*> o .:? "entities"
                    <*> o .:  "user"
                    <*> o .:  "retweeted_status"
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
    return (ETUnknown v)
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

data User =
  User
  { userId              :: UserId
  , userName            :: UserName
  , userScreenName      :: String
  , userDescription     :: Maybe T.Text
  , userLocation        :: Maybe T.Text
  , userProfileImageURL :: Maybe URLString
  , userURL             :: Maybe URLString
  , userProtected       :: Maybe Bool
  , userFollowers       :: Maybe Int
  , userFriends         :: Maybe Int
  , userTweets          :: Maybe Int
  , userLangCode        :: Maybe LanguageCode
  , userCreatedAt       :: Maybe DateString
  } deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Object o) = checkError o <|>
    User <$> o .:  "id"
         <*> o .:  "name"
         <*> o .:  "screen_name"
         <*> o .:? "description"
         <*> o .:? "location"
         <*> o .:? "profile_image_url"
         <*> o .:? "url"
         <*> o .:? "protected"
         <*> o .:? "followers_count"
         <*> o .:? "friends_count"
         <*> o .:? "statuses_count"
         <*> o .:? "lang"
         <*> o .:? "created_at"
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
    List <$> o .:  "id"
         <*> o .:  "name"
         <*> o .:  "full_name"
         <*> o .:  "member_count"
         <*> o .:  "subscriber_count"
         <*> o .:  "mode"
         <*> o .:  "user"
  parseJSON _ = mzero

data HashTagEntity =
  HashTagEntity
  { hashTagText :: T.Text -- ^ The Hashtag text
  } deriving (Show, Eq)

instance FromJSON HashTagEntity where
  parseJSON (Object o) =
    HashTagEntity <$> o .: "text"
  parseJSON _ = mzero

-- | The 'UserEntity' is just a wrapper around 'User' which is
--   a bit wasteful, and should probably be replaced by just
--   storing the id, name and screen name here.
data UserEntity = UserEntity User
                deriving (Show, Eq)

instance FromJSON UserEntity where
  parseJSON = (UserEntity <$>) . parseJSON

data URLEntity =
  URLEntity
  { ueURL      :: URLString -- ^ The URL that was extracted
  , ueExpanded :: URLString -- ^ The fully resolved URL (only for t.co links)
  , ueDisplay  :: T.Text    -- ^ Not a URL but a string to display instead of the URL (only for t.co links)
  } deriving (Show, Eq)

instance FromJSON URLEntity where
  parseJSON (Object o) =
    URLEntity <$> o .:  "url"
              <*> o .:  "expanded_url"
              <*> o .:  "display_url"
  parseJSON _ = mzero

-- | Entity handling.
data Entities =
  Entities
  { enHashTags     :: [Entity HashTagEntity]
  , enUserMentions :: [Entity UserEntity]
  , enURLs         :: [Entity URLEntity]
  } deriving (Show, Eq)

instance FromJSON Entities where
  parseJSON (Object o) =
    Entities <$> o .:  "hashtags"
             <*> o .:  "user_mentions"
             <*> o .:  "urls"
  parseJSON _ = mzero

-- | The character positions the Entity was extracted from
--
--   This is experimental implementation.
--   This may be replaced by more definite types.
type EntityIndices = [Int]

data Entity a =
  Entity
  { entityBody    :: a             -- ^ The detail information of the specific entity types (HashTag, URL, User)
  , entityIndices :: EntityIndices -- ^ The character positions the Entity was extracted from
  } deriving (Show, Eq)

instance FromJSON a => FromJSON (Entity a) where
  parseJSON v@(Object o) =
    Entity <$> parseJSON v
           <*> o .: "indices"
  parseJSON _ = mzero

