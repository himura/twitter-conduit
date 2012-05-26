{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

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
import Web.Twitter.Conduit.Param
import Web.Twitter.Conduit.Api

statuses :: (TwitterBaseM m, FromJSON a)
         => AuthHandler cred m -- ^ OAuth required?
         -> String -- ^ Resource URL
         -> HT.Query -- ^ Query
         -> C.Source (TW cred m) a
statuses hndl uri query = apiWithPages hndl furi query
  where furi = endpoint ++ "statuses/" ++ uri

statusesHomeTimeline :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
statusesHomeTimeline = statuses authRequired "home_timeline.json"

statusesMentions :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
statusesMentions = statuses authRequired "mentions.json"

statusesPublicTimeline :: TwitterBaseM m => HT.Query -> C.Source (TW NoToken m) Status
statusesPublicTimeline = statuses noAuth "public_timeline.json"

statusesRetweetedByMe :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
statusesRetweetedByMe = statuses authRequired "retweeted_by_me.json"

statusesRetweetedToMe :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
statusesRetweetedToMe = statuses authRequired "retweeted_to_me.json"

statusesRetweetsOfMe :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
statusesRetweetsOfMe = statuses authRequired "retweeted_of_me.json"

statusesUserTimeline :: TwitterBaseM m => HT.Query -> C.Source (TW cred m) Status
statusesUserTimeline = statuses authSupported "user_timeline.json"

statusesRetweetedToUser :: TwitterBaseM m => HT.Query -> C.Source (TW cred m) Status
statusesRetweetedToUser = statuses authSupported "retweeted_to_user.json"

statusesRetweetedByUser :: TwitterBaseM m => HT.Query -> C.Source (TW cred m) Status
statusesRetweetedByUser = statuses authSupported "retweeted_by_user.json"

statusesIdRetweetedBy :: TwitterBaseM m => StatusId -> HT.Query -> C.Source (TW cred m) User
statusesIdRetweetedBy status_id = statuses authSupported (show status_id ++ "/retweeted_by.json")

statusesIdRetweetedByIds :: TwitterBaseM m => StatusId -> HT.Query -> C.Source (TW WithToken m) UserId
statusesIdRetweetedByIds status_id = statuses authRequired (show status_id ++ "/retweeted_by/ids.json")

statusesRetweetsId :: TwitterBaseM m => StatusId -> HT.Query -> (TW WithToken m) [RetweetedStatus]
statusesRetweetsId status_id query = apiGet authRequired uri query
  where uri = endpoint ++ "statuses/retweets/" ++ show status_id ++ ".json"

statusesShowId :: TwitterBaseM m => StatusId -> HT.Query -> (TW WithToken m) Status
statusesShowId status_id query = apiGet authSupported (endpoint ++ "statuses/show/" ++ show status_id ++ ".json") query

search :: TwitterBaseM m => String -> C.Source (TW NoToken m) SearchStatus
search q = apiWithPages noAuth (endpointSearch ++ "search.json") query
  where query = [("q", Just . B8.pack $ q)]


friendsIds, followersIds
  :: TwitterBaseM m => UserParam -> C.Source (TW cred m) UserId
friendsIds   q = apiCursor authSupported "friends/ids.json"   (mkUserParam q) "ids"
followersIds q = apiCursor authSupported "followers/ids.json" (mkUserParam q) "ids"

usersShow :: TwitterBaseM m => UserParam -> (TW cred m) User
usersShow q = apiGet authSupported "users/show.json" (mkUserParam q)

listsAll :: TwitterBaseM m => UserParam -> C.Source (TW cred m) List
listsAll q = apiCursor authSupported "lists/all.json" (mkUserParam q) ""

listsMembers :: TwitterBaseM m => ListParam -> C.Source (TW cred m) User
listsMembers q = apiCursor authSupported "lists/members.json" (mkListParam q) "users"

