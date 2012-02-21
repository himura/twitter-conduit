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
       , RetweetCount
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
       , HashTagEntity(..)
       , UserEntity(..)
       , URLEntity(..)
       , checkError
       )
       where

import qualified Network.HTTP.Types as HT
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Attoparsec.Number (Number(..))
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

type DateString   = String
type UserId       = Integer
type Friends      = [UserId]
type URLString    = String
type UserName     = T.Text
type StatusId     = Integer
type LanguageCode = String

-- | The re-tweeted count appears to be limited to 100;
--   any values larger than this are listed as a string
--   as \"100+\". It is not clear from the documentation
--   whether you can get a 100 and 100+ or just the latter.
--
--   For now we treat the re-tweet cound as a bounded value
--   between 0 and 100 inclusive, but this is an
--   /experimental/ change.
--
newtype RetweetCount = RC Int
                       deriving Eq
                                
instance Show RetweetCount where
  show (RC i) = show i
  
instance Bounded RetweetCount where
  minBound = RC 0
  maxBound = RC 100

instance Enum RetweetCount where
  toEnum i | i >= 0 && i <= 100 = RC i
           | otherwise          = error "Integer value outside range of RetweetCount"
  fromEnum (RC i) = i

instance FromJSON RetweetCount where
  parseJSON (Number (I n)) | n >= 0 && n <= 100 = return $ RC $ fromIntegral n
                           | otherwise          = mzero
  parseJSON (String s)     | s == "100+" = return maxBound
                           | otherwise   = mzero
  parseJSON _ = mzero

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
  , statusEntities      :: Entities
  , statusInReplyTo     :: Maybe StatusId
  , statusInReplyToUser :: Maybe UserId
  , statusFavorite      :: Maybe Bool
  , statusRetweetCount  :: Maybe RetweetCount
  , statusUser          :: User
  } deriving (Show, Eq)

instance FromJSON Status where
  parseJSON (Object o) = checkError o <|>
    Status <$> o .:  "created_at"
           <*> o .:  "id"
           <*> o .:  "text"
           <*> o .:  "source"
           <*> o .:  "truncated"
           <*> o .:  "entities"
           <*> o .:? "in_reply_to_status_id"
           <*> o .:? "in_reply_to_user_id"
           -- <*> o .:? "favorite" -- TODO: check whether it should be favorite or favorited
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
  , rsEntities        :: Entities
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
                    <*> o .: "entities"
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
    List <$> o .: "id"
         <*> o .: "name"
         <*> o .: "full_name"
         <*> o .: "member_count"
         <*> o .: "subscriber_count"
         <*> o .: "mode"
         <*> o .: "user"
  parseJSON _ = mzero

data HashTagEntity = HashTagEntity T.Text
                   deriving (Show, Eq)
                        
instance FromJSON HashTagEntity where
  parseJSON (Object o) = 
    HashTagEntity <$> o .: "text"
  parseJSON _ = mzero
  
{-|
The 'UserEntity' is just a wrapper around 'User' which is
a bit wasteful, and should probably be replaced by just
storing the id, name and screen name here.
-}
data UserEntity = UserEntity User
                deriving (Show, Eq)

instance FromJSON UserEntity where
  parseJSON = (UserEntity <$>) . parseJSON
  
data URLEntity = 
  URLEntity
  { ueURL      :: URLString
  , ueExpanded :: URLString
  , ueDisplay  :: T.Text
  } deriving (Show, Eq)
             
instance FromJSON URLEntity where
  parseJSON (Object o) = 
    URLEntity <$> o .:  "url"
              <*> o .:  "expanded_url"
              <*> o .:  "display_url"
  parseJSON _ = mzero

{-|
Entity handling. At present the information about where
in the Tweet the entity was found (the @indices@ information)
is not retained.
-}

data Entities = 
  Entities 
  { enHashTags     :: [HashTagEntity]
  , enUserMentions :: [UserEntity]
  , enURLs         :: [URLEntity]
  } deriving (Show, Eq)
             
instance FromJSON Entities where
  parseJSON (Object o) = 
    Entities <$> o .:  "hashtags"
             <*> o .:  "user_mentions"
             <*> o .:  "urls"
  parseJSON _ = mzero
             
