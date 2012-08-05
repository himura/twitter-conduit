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
       , showId
       ) where

import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Types

import qualified Network.HTTP.Types as HT
import Data.Aeson
import qualified Data.Conduit as C

statuses :: (TwitterBaseM m, FromJSON a)
         => AuthHandler cred m -- ^ OAuth required?
         -> String -- ^ Resource URL
         -> HT.SimpleQuery -- ^ Query
         -> C.Source (TW cred m) a
statuses hndl uri query = apiWithPages hndl u query
  where u = "statuses/" ++ uri

homeTimeline :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW WithToken m) Status
homeTimeline = statuses authRequired "home_timeline.json"

mentions :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW WithToken m) Status
mentions = statuses authRequired "mentions.json"

publicTimeline :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW cred m) Status
publicTimeline = statuses noAuth "public_timeline.json"

retweetedByMe :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW WithToken m) Status
retweetedByMe = statuses authRequired "retweeted_by_me.json"

retweetedToMe :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW WithToken m) Status
retweetedToMe = statuses authRequired "retweeted_to_me.json"

retweetsOfMe :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW WithToken m) Status
retweetsOfMe = statuses authRequired "retweeted_of_me.json"

userTimeline :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW cred m) Status
userTimeline = statuses authSupported "user_timeline.json"

retweetedToUser :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW cred m) Status
retweetedToUser = statuses authSupported "retweeted_to_user.json"

retweetedByUser :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW cred m) Status
retweetedByUser = statuses authSupported "retweeted_by_user.json"

idRetweetedBy :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> C.Source (TW cred m) User
idRetweetedBy status_id = statuses authSupported (show status_id ++ "/retweeted_by.json")

idRetweetedByIds :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> C.Source (TW WithToken m) UserId
idRetweetedByIds status_id = statuses authRequired (show status_id ++ "/retweeted_by/ids.json")

retweetsId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> (TW WithToken m) [RetweetedStatus]
retweetsId status_id query = apiGet authRequired uri query
  where uri = "statuses/retweets/" ++ show status_id ++ ".json"

showId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> (TW WithToken m) Status
showId status_id query = apiGet authSupported uri query
  where uri = "statuses/show/" ++ show status_id ++ ".json"
