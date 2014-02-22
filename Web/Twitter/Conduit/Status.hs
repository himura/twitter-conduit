{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Status
       (
       -- * Timelines
         mentionsTimeline
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

import Web.Twitter.Conduit.Base
import Web.Twitter.Conduit.Request
import Web.Twitter.Types

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Conduit
import Data.Default

-- * Timelines

data StatusesMentionsTimeline
mentionsTimeline :: APIRequest StatusesMentionsTimeline [Status]
mentionsTimeline = APIRequestGet (endpoint ++ "statuses/mentions_timeline.json") def

data StatusesUserTimeline
userTimeline :: APIRequest StatusesUserTimeline [Status]
userTimeline = APIRequestGet (endpoint ++ "statuses/user_timeline.json") def
instance HasMaxIdParam (APIRequest StatusesUserTimeline [Status])

data StatusesHomeTimeline
homeTimeline :: APIRequest StatusesHomeTimeline [Status]
homeTimeline = APIRequestGet (endpoint ++ "statuses/home_timeline.json") def
instance HasMaxIdParam (APIRequest StatusesHomeTimeline [Status])

data StatusesRetweetsOfMe
retweetsOfMe :: APIRequest StatusesRetweetsOfMe [Status]
retweetsOfMe = APIRequestGet (endpoint ++ "statuses/retweets_of_me.json") def

-- * Tweets

data StatusesRetweetsId
retweetsId :: StatusId -> APIRequest StatusesRetweetsId [RetweetedStatus]
retweetsId status_id = APIRequestGet uri def
  where uri = endpoint ++ "statuses/retweets/" ++ show status_id ++ ".json"

data StatusesShowId
showId :: StatusId -> APIRequest StatusesShowId Status
showId status_id = APIRequestGet uri def
  where uri = endpoint ++ "statuses/show/" ++ show status_id ++ ".json"

data StatusesDestroyId
destroyId :: StatusId -> APIRequest StatusesDestroyId Status
destroyId status_id = APIRequestPost uri def
  where uri = endpoint ++ "statuses/destroy/" ++ show status_id ++ ".json"

data StatusesUpdate
update :: T.Text -> APIRequest StatusesUpdate Status
update status = APIRequestPost uri [("status", T.encodeUtf8 status)]
  where uri = endpoint ++ "statuses/update.json"

data StatusesRetweetId
retweetId :: StatusId -> APIRequest StatusesRetweetId RetweetedStatus
retweetId status_id = APIRequestPost uri def
  where uri = "statuses/retweet/" ++ show status_id ++ ".json"

data MediaData = MediaFromFile FilePath
               | MediaRequestBody FilePath RequestBody

data StatusesUpdateWithMedia
updateWithMedia :: T.Text
                -> MediaData
                -> APIRequest StatusesUpdateWithMedia Status
updateWithMedia tweet mediaData =
    APIRequestPostMultipart uri [("status", T.encodeUtf8 tweet)] [mediaBody mediaData]
  where
    uri = endpoint ++ "statuses/update_with_media.json"
    mediaBody (MediaFromFile fp) = partFileSource "media[]" fp
    mediaBody (MediaRequestBody filename filebody) = partFileRequestBody "media[]" filename filebody
