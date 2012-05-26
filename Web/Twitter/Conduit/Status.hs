{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Status
       ( statuses
       -- * Timelines
       , homeTimeline
       , mentions
       , publicTimeline
       , retweetedByMe
       , retweetedToMe
       , retweetsOfMe
       , userTimeline
       , retweetedToUser
       , retweetedByUser
       , idRetweetedBy
       , idRetweetedByIds
       , retweetsId
       ) where

import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Types

import qualified Network.HTTP.Types as HT
import Data.Aeson
import qualified Data.Conduit as C

statuses :: (TwitterBaseM m, FromJSON a) => String -> HT.Query -> C.Source (TW WithToken m) a
statuses url query = apiWithPages authRequired ("statuses/" ++ url) query

homeTimeline :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
homeTimeline = statuses "home_timeline.json"

mentions :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
mentions = statuses "mentions.json"

publicTimeline :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
publicTimeline = statuses "public_timeline.json"

retweetedByMe :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
retweetedByMe = statuses "retweeted_by_me.json"

retweetedToMe :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
retweetedToMe = statuses "retweeted_to_me.json"

retweetsOfMe :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
retweetsOfMe = statuses "retweeted_of_me.json"

userTimeline :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
userTimeline = statuses "user_timeline.json"

retweetedToUser :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
retweetedToUser = statuses "retweeted_to_user.json"

retweetedByUser :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) Status
retweetedByUser = statuses "retweeted_by_user.json"

idRetweetedBy :: TwitterBaseM m => StatusId -> HT.Query -> C.Source (TW WithToken m) User
idRetweetedBy status_id = statuses (show status_id ++ "/retweeted_by.json")

idRetweetedByIds :: TwitterBaseM m => StatusId -> HT.Query -> C.Source (TW WithToken m) UserId
idRetweetedByIds status_id = statuses (show status_id ++ "/retweeted_by/ids.json")

retweetsId :: TwitterBaseM m => StatusId -> HT.Query -> TW WithToken m [RetweetedStatus]
retweetsId status_id query = apiGet authRequired uri query
  where uri = "statuses/retweets/" ++ show status_id ++ ".json"

