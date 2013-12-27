{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Status
       ( statuses
       -- * Timelines
       , mentionsTimeline
       , userTimeline
       , homeTimeline
       , retweetsOfMe
       -- * Tweets
       , retweetsId
       , showId
       , destroyId
       , update
       , retweetId
       -- , updateWithMedia
       -- , oembed
       ) where

import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Monad
import Web.Twitter.Types

import qualified Network.HTTP.Types as HT
import Data.Aeson
import qualified Data.Conduit as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

statuses :: (TwitterBaseM m, FromJSON a)
         => String -- ^ Resource URL
         -> HT.SimpleQuery -- ^ Query
         -> C.Source (TW m) a
statuses uri query = apiWithPages u query
  where u = "statuses/" ++ uri

-- * Timelines

mentionsTimeline :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
mentionsTimeline = statuses "mentions_timeline.json"

userTimeline :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
userTimeline = statuses "user_timeline.json"

homeTimeline :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
homeTimeline = statuses "home_timeline.json"

retweetsOfMe :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
retweetsOfMe = statuses "retweets_of_me.json"

-- * Tweets

retweetsId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m [RetweetedStatus]
retweetsId status_id query = apiGet uri query
  where uri = "statuses/retweets/" ++ show status_id ++ ".json"

showId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
showId status_id query = apiGet uri query
  where uri = "statuses/show/" ++ show status_id ++ ".json"

destroyId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
destroyId status_id query = apiPost uri query
  where uri = "statuses/destroy/" ++ show status_id ++ ".json"

update :: TwitterBaseM m => T.Text -> HT.SimpleQuery -> TW m Status
update status query = apiPost "statuses/update.json" (("status", T.encodeUtf8 status):query)

retweetId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m RetweetedStatus
retweetId status_id query = apiPost uri query
  where uri = "statuses/retweet/" ++ show status_id ++ ".json"

-- updateWithMedia
-- oembed
-- retweetersIds
