{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Conduit.Fetch (
       -- * Timelines
         statusesHomeTimeline
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
       , search
  
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
  ) where

--import Control.Applicative
import qualified Data.Conduit as C
import qualified Data.ByteString.Char8 as B8
import qualified Network.HTTP.Types as HT
import Data.Aeson hiding (Error)
--import qualified Data.Aeson.Types as AE
--import qualified Data.Map as M

import Web.Twitter.Conduit.Types
import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Utils
import Web.Twitter.Conduit.Query
import Web.Twitter.Conduit.Api

mkQueryUser :: QueryUser -> HT.Query
mkQueryUser (QUserId uid) =  [("user_id", Just $ showBS uid)]
mkQueryUser (QScreenName sn) = [("screen_name", Just . B8.pack $ sn)]

mkQueryList :: QueryList -> HT.Query
mkQueryList (QListId lid) =  [("list_id", Just $ showBS lid)]
mkQueryList (QListName listname) =
  [("slug", Just . B8.pack $ lstName),
   ("owner_screen_name", Just . B8.pack $ screenName)]
  where
    (screenName, ln) = span (/= '/') listname
    lstName = drop 1 ln

statuses :: (FromJSON a, Show a) => String -> HT.Query -> C.Source TW a
statuses uri query = apiWithPages furi query
  where furi = endpoint ++ "statuses/" ++ uri

--iterPageC :: (Monad m, FromJSON a) => C.Sink Value m (Maybe [a])
--iterPageC = do
--  ret <- C.await
--  return $ ret >>= fromJSON'

--iterPageCSearch :: (Monad m, FromJSON a) => C.Sink Value m (Maybe [a])
--iterPageCSearch = fromJSONSearch' <$> C.await

--insertQuery :: B8.ByteString -> Maybe B8.ByteString -> HT.Query -> HT.Query
--insertQuery key value = mk
--  where mk = M.toList . M.insert key value . M.fromList

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
statusesRetweetsId status_id query = apiGet uri query
  where uri = endpoint ++ "statuses/retweets/" ++ show status_id ++ ".json"

statusesShowId :: StatusId -> HT.Query -> TW Status
statusesShowId status_id query = apiGet (endpoint ++ "statuses/show/" ++ show status_id ++ ".json") query

search :: String -> C.Source TW SearchStatus
search q = apiWithPages (endpointSearch ++ "search.json") query
  where query = [("q", Just . B8.pack $ q)]


friendsIds, followersIds :: QueryUser -> C.Source TW UserId
friendsIds   q = apiCursor "friends/ids.json"   (mkQueryUser q) "ids"
followersIds q = apiCursor "followers/ids.json" (mkQueryUser q) "ids"

usersShow :: QueryUser -> TW User
usersShow q = apiGet "users/show.json" (mkQueryUser q)

listsAll :: QueryUser -> C.Source TW List
listsAll q = apiCursor "lists/all.json" (mkQueryUser q) ""

listsMembers :: QueryList -> C.Source TW User
listsMembers q = apiCursor "lists/members.json" (mkQueryList q) "users"

