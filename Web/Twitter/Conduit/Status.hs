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
       , MediaData (..)
       , updateWithMedia
       -- , oembed
       -- , retweetersIds
       ) where

import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Utils
import Web.Twitter.Types

import qualified Network.HTTP.Types as HT
import Data.Aeson
import qualified Data.Conduit as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Conduit
import Data.Conduit
import Control.Arrow

statuses :: (TwitterBaseM m, FromJSON a)
         => String -- ^ Resource URL
         -> HT.SimpleQuery -- ^ Query
         -> C.Source (TW m) a
statuses uri = apiWithPages u
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
retweetsId status_id = apiGet uri
  where uri = "statuses/retweets/" ++ show status_id ++ ".json"

showId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
showId status_id = apiGet uri
  where uri = "statuses/show/" ++ show status_id ++ ".json"

destroyId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
destroyId status_id = apiPost uri
  where uri = "statuses/destroy/" ++ show status_id ++ ".json"

update :: TwitterBaseM m => T.Text -> HT.SimpleQuery -> TW m Status
update status query = apiPost "statuses/update.json" (("status", T.encodeUtf8 status):query)

retweetId :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m RetweetedStatus
retweetId status_id = apiPost uri
  where uri = "statuses/retweet/" ++ show status_id ++ ".json"

data MediaData = MediaFromFile FilePath
               | MediaRequestBody FilePath RequestBody

updateWithMedia :: TwitterBaseM m
                => T.Text
                -> MediaData
                -> HT.SimpleQuery
                -> TW m Status
updateWithMedia tweet mediaData query = do
    req <- formDataBody body =<< makeRequest "POST" (endpoint ++ "statuses/update_with_media.json") []
    res <- apiRequest req
    res $$+- sinkFromJSON
  where
    body = mediaBody mediaData : partQuery

    partQuery = map (uncurry partBS . first T.decodeUtf8) $ ("status", T.encodeUtf8 tweet) : query
    mediaBody (MediaFromFile fp) = partFileSource "media[]" fp
    mediaBody (MediaRequestBody filename filebody) = partFileRequestBody "media[]" filename filebody
