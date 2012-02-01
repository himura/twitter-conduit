{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Fetch
       ( QueryUser(..)
       , QueryList(..)

       -- * Timelines
       , statusesHomeTimeline
       , statusesMentions
       , statusesPublicTimeline
       , statusesRetweetedByMe
       , statusesRetweetedToMe
       , statusesRetweetsOfMe
       , statusesUserTimeline
       , statusesRetweetedToUser
       , statusesRetweetedByUser

       -- * Tweets
       , statusesIdRetweetedBy
       , statusesIdRetweetedByIds
       , statusesRetweetsId
       , statusesShowId

       -- * Search
       -- , search

       -- * Direct Messages
       -- , directMessages
       -- , directMessagesSent
       -- , directMessagesShowId

       -- * Friends & Followers
       , friendsIds
       , followersIds
       -- , friendshipsExists
       -- , friendshipsIncoming
       -- , friendshipsOutgoing
       -- , friendshipsShow
       -- , friendshipsLookup
       -- , friendshipsNoRetweetIds

       -- * Users
       -- , usersLookup
       -- , usersProfileImageScreenName
       -- , usersSearch
       , usersShow
       -- , usersContributees
       -- , usersContributors

       -- * Suggested Users
       -- , usersSuggestions
       -- , usersSuggestionsSlug
       -- , usersSuggestionsSlugMembers

       -- * Favorites
       -- , favorites

       -- * Lists
       , listsAll
       -- , listsStatuses
       -- , listsMemberships
       -- , listsSubscribers
       -- , listsSubscribersShow
       -- , listsMembersShow
       , listsMembers
       -- , lists
       -- , listsShow
       -- , listsSubscriptions

       -- * StreamingAPI
       , userstream
       , statusesFilter
       )
       where

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Applicative
import Data.Aeson hiding (Error)
import qualified Data.Aeson.Types as AE
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

import Web.Twitter.Types
import Web.Twitter.Monad
import Web.Twitter.Utils
import Web.Twitter.Api

data QueryUser = QUserId UserId | QScreenName String
               deriving (Show, Eq)
data QueryList = QListId Integer | QListName String
               deriving (Show, Eq)

apiGet :: FromJSON a => String -> HT.Query -> TW a
apiGet uri query = runResourceT $ do
  src <- api "GET" uri query    
  src C.$$ sinkJSON'

statuses :: (FromJSON a, Show a) => String -> HT.Query -> C.Source TW a
statuses uri query = apiWithPages furi query 1
  where furi = endpoint ++ "statuses/" ++ uri

apiWithPages :: (FromJSON a, Show a) => String -> HT.Query -> Integer -> C.Source TW a
apiWithPages uri query initPage = undefined
{-
apiWithPages uri query initPage =
  checkContinue1 go initPage
  where
    go loop page k = do
      let query' = insertQuery "page" (toMaybeByteString page) query
      res <- lift $ run_ $ api "GET" uri query' (handleParseError (enumJSON =$ iterPageC))
      case res of
        Just [] -> k EOF
        Just xs -> k (Chunks xs) >>== loop (page + 1)
        Nothing -> k EOF

iterPageC :: (C.Resource m, Monad m, FromJSON a) => C.Sink Value m (Maybe [a])
iterPageC = do
  ret <- CL.head
  case ret of
    Just v -> return . fromJSON' $ v
    Nothing -> return Nothing
-}

insertQuery :: ByteString -> Maybe ByteString -> HT.Query -> HT.Query
insertQuery key value = mk
  where mk = M.toList . M.insert key value . M.fromList

--

statusesHomeTimeline :: HT.Query -> C.Source TW Status
statusesHomeTimeline = statuses "home_timeline.json"

statusesMentions :: HT.Query -> C.Source TW Status
statusesMentions = statuses "mentions.json"

statusesPublicTimeline :: HT.Query -> C.Source TW Status
statusesPublicTimeline = statuses "public_timeline.json"

statusesRetweetedByMe :: HT.Query -> C.Source TW Status
statusesRetweetedByMe = statuses "retweeted_by_me.json"

statusesRetweetedToMe :: HT.Query -> C.Source TW Status
statusesRetweetedToMe = statuses "retweeted_to_me.json"

statusesRetweetsOfMe :: HT.Query -> C.Source TW Status
statusesRetweetsOfMe = statuses "retweeted_of_me.json"

statusesUserTimeline :: HT.Query -> C.Source TW Status
statusesUserTimeline = statuses "user_timeline.json"

statusesRetweetedToUser :: HT.Query -> C.Source TW Status
statusesRetweetedToUser = statuses "retweeted_to_user.json"

statusesRetweetedByUser :: HT.Query -> C.Source TW Status
statusesRetweetedByUser = statuses "retweeted_by_user.json"

statusesIdRetweetedBy :: StatusId -> HT.Query -> C.Source TW User
statusesIdRetweetedBy status_id = statuses (show status_id ++ "/retweeted_by.json")

statusesIdRetweetedByIds :: StatusId -> HT.Query -> C.Source TW UserId
statusesIdRetweetedByIds status_id = statuses (show status_id ++ "/retweeted_by/ids.json")

statusesRetweetsId :: StatusId -> HT.Query -> TW [RetweetedStatus]
statusesRetweetsId status_id query = undefined -- apiGet uri query CL.head_
  -- where uri = endpoint ++ "statuses/retweets/" ++ show status_id ++ ".json"

statusesShowId :: StatusId -> HT.Query -> TW Status
statusesShowId status_id query = undefined -- apiGet (endpoint ++ "statuses/show/" ++ show status_id ++ ".json") query CL.head_

mkQueryUser :: QueryUser -> HT.Query
mkQueryUser (QUserId uid) =  [("user_id", toMaybeByteString uid)]
mkQueryUser (QScreenName sn) = [("screen_name", Just . B8.pack $ sn)]

mkQueryList :: QueryList -> HT.Query
mkQueryList (QListId lid) =  [("list_id", toMaybeByteString lid)]
mkQueryList (QListName listname) =
  [("slug", Just . B8.pack $ lstName),
   ("owner_screen_name", Just . B8.pack $ screenName)]
  where
    (screenName, ln) = span (/= '/') listname
    lstName = drop 1 ln

friendsIds, followersIds :: QueryUser -> C.ResourceT TW (C.Source IO UserId)
friendsIds   q = apiCursor (endpoint ++ "friends/ids.json")   (mkQueryUser q) "ids"
followersIds q = apiCursor (endpoint ++ "followers/ids.json") (mkQueryUser q) "ids"

usersShow :: QueryUser -> TW User
usersShow q = undefined -- apiGet (endpoint ++ "users/show.json") (mkQueryUser q) CL.head_

listsAll :: QueryUser -> C.Source TW List
listsAll q = undefined -- apiCursor (endpoint ++ "lists/all.json") (mkQueryUser q) ""

listsMembers :: QueryList -> C.Source TW User
listsMembers q = undefined -- apiCursor (endpoint ++ "lists/members.json") (mkQueryList q) "users"

apiCursor :: (FromJSON a, C.ResourceThrow m)
             => String
             -> HT.Query
             -> T.Text
             -> C.ResourceT TW (C.Source m a)
apiCursor uri query cursorKey = go (-1) where
  go cursor = do
    let query' = insertQuery "cursor" (toMaybeByteString (cursor :: Int)) query
    res <- api "GET" uri query'
    j <- res C.$$ sinkJSON
    case AE.parseMaybe p j of
      Nothing ->
        return CL.sourceNull
      Just (res, 0) ->
        return $ CL.sourceList res
      Just (res, nextCursor) ->
        mappend (CL.sourceList res) <$> go nextCursor

  p (Object v) = (,) <$> v .: cursorKey <*> v .: "next_cursor"
  p _ = mempty

streamingConduit :: C.ResourceThrow m => C.Conduit ByteString m StreamingAPI
streamingConduit = conduitParser parseFromJSON

userstream :: C.ResourceT TW (C.Source TW StreamingAPI)
userstream = do
  src <- api "GET" "https://userstream.twitter.com/2/user.json" []
  return $ src C.$= streamingConduit

statusesFilter :: HT.Query -> C.ResourceT TW (C.Source TW StreamingAPI)
statusesFilter query = do
  src <- api "GET" "https://stream.twitter.com/1/statuses/filter.json" query
  return $ src C.$= streamingConduit

toMaybeByteString :: Show a => a -> Maybe ByteString
toMaybeByteString = Just . B8.pack . show
