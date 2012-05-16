{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Conduit.Fetch
       (
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

statuses :: (FromJSON a, Show a)
         => RequireAuth -- ^ OAuth required?
         -> String -- ^ Resource URL
         -> HT.Query -- ^ Query
         -> C.Source TW a
statuses authp uri query = apiWithPages authp furi query
  where furi = endpoint ++ "statuses/" ++ uri

statusesHomeTimeline :: HT.Query -> C.Source TW Status
statusesHomeTimeline = statuses AuthRequired "home_timeline.json"

statusesMentions :: HT.Query -> C.Source TW Status
statusesMentions = statuses AuthRequired "mentions.json"

statusesPublicTimeline :: HT.Query -> C.Source TW Status
statusesPublicTimeline = statuses NoAuth "public_timeline.json"

statusesRetweetedByMe :: HT.Query -> C.Source TW Status
statusesRetweetedByMe = statuses AuthRequired "retweeted_by_me.json"

statusesRetweetedToMe :: HT.Query -> C.Source TW Status
statusesRetweetedToMe = statuses AuthRequired "retweeted_to_me.json"

statusesRetweetsOfMe :: HT.Query -> C.Source TW Status
statusesRetweetsOfMe = statuses AuthRequired "retweeted_of_me.json"

statusesUserTimeline :: HT.Query -> C.Source TW Status
statusesUserTimeline = statuses AuthSupported "user_timeline.json"

statusesRetweetedToUser :: HT.Query -> C.Source TW Status
statusesRetweetedToUser = statuses AuthSupported "retweeted_to_user.json"

statusesRetweetedByUser :: HT.Query -> C.Source TW Status
statusesRetweetedByUser = statuses AuthSupported "retweeted_by_user.json"

statusesIdRetweetedBy :: StatusId -> HT.Query -> C.Source TW User
statusesIdRetweetedBy status_id = statuses AuthSupported (show status_id ++ "/retweeted_by.json")

statusesIdRetweetedByIds :: StatusId -> HT.Query -> C.Source TW UserId
statusesIdRetweetedByIds status_id = statuses AuthRequired (show status_id ++ "/retweeted_by/ids.json")

statusesRetweetsId :: StatusId -> HT.Query -> TW [RetweetedStatus]
statusesRetweetsId status_id query = apiGet AuthRequired uri query
  where uri = endpoint ++ "statuses/retweets/" ++ show status_id ++ ".json"

statusesShowId :: StatusId -> HT.Query -> TW Status
statusesShowId status_id query = apiGet AuthSupported (endpoint ++ "statuses/show/" ++ show status_id ++ ".json") query

search :: String -> C.Source TW SearchStatus
search q = apiWithPages NoAuth (endpointSearch ++ "search.json") query
  where query = [("q", Just . B8.pack $ q)]


friendsIds, followersIds :: QueryUser -> C.Source TW UserId
friendsIds   q = apiCursor AuthSupported "friends/ids.json"   (mkQueryUser q) "ids"
followersIds q = apiCursor AuthSupported "followers/ids.json" (mkQueryUser q) "ids"

usersShow :: QueryUser -> TW User
usersShow q = apiGet AuthSupported "users/show.json" (mkQueryUser q)

listsAll :: QueryUser -> C.Source TW List
listsAll q = apiCursor AuthSupported "lists/all.json" (mkQueryUser q) ""

listsMembers :: QueryList -> C.Source TW User
listsMembers q = apiCursor AuthSupported "lists/members.json" (mkQueryList q) "users"

