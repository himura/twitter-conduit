{-# LANGUAGE OverloadedStrings #-}
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
       , destroyId
       , retweetId
       , update
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
         => AuthHandler m -- ^ OAuth handler
         -> String -- ^ Resource URL
         -> HT.SimpleQuery -- ^ Query
         -> C.Source (TW m) a
statuses hndl uri query = apiWithPages hndl u query
  where u = "statuses/" ++ uri

homeTimeline :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
homeTimeline = statuses signOAuthTW "home_timeline.json"

mentions :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
mentions = statuses signOAuthTW "mentions.json"

publicTimeline :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
publicTimeline = statuses signOAuthTW "public_timeline.json"

retweetedByMe :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
retweetedByMe = statuses signOAuthTW "retweeted_by_me.json"

retweetedToMe :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
retweetedToMe = statuses signOAuthTW "retweeted_to_me.json"

retweetsOfMe :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
retweetsOfMe = statuses signOAuthTW "retweeted_of_me.json"

userTimeline :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
userTimeline = statuses signOAuthTW "user_timeline.json"

retweetedToUser :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
retweetedToUser = statuses signOAuthTW "retweeted_to_user.json"

retweetedByUser :: TwitterBaseM m => HT.SimpleQuery -> C.Source (TW m) Status
retweetedByUser = statuses signOAuthTW "retweeted_by_user.json"

idRetweetedBy :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> C.Source (TW m) User
idRetweetedBy status_id = statuses signOAuthTW (show status_id ++ "/retweeted_by.json")

idRetweetedByIds :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> C.Source (TW m) UserId
idRetweetedByIds status_id = statuses signOAuthTW (show status_id ++ "/retweeted_by/ids.json")

retweetsId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m [RetweetedStatus]
retweetsId status_id query = apiGet signOAuthTW uri query
  where uri = "statuses/retweets/" ++ show status_id ++ ".json"

showId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
showId status_id query = apiGet signOAuthTW uri query
  where uri = "statuses/show/" ++ show status_id ++ ".json"

destroyId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
destroyId status_id query = apiPost signOAuthTW uri query
  where uri = "statuses/destroy/" ++ show status_id ++ ".json"

retweetId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m RetweetedStatus
retweetId status_id query = apiPost signOAuthTW uri query
  where uri = "statuses/retweet/" ++ show status_id ++ ".json"

update :: TwitterBaseM m => T.Text -> HT.SimpleQuery -> TW m Status
update status query = apiPost signOAuthTW "statuses/update.json" (("status", T.encodeUtf8 status):query)
