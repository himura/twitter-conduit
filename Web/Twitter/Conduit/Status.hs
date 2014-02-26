{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Web.Twitter.Conduit.Param
import Web.Twitter.Conduit.Parameters.TH
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
deriveHasParamInstances ''StatusesMentionsTimeline
    [ "count"
    , "since_id"
    , "max_id"
    , "trim_user"
    , "contributor_details"
    , "include_entities"
    ]

data StatusesUserTimeline
userTimeline :: UserParam -> APIRequest StatusesUserTimeline [Status]
userTimeline q = APIRequestGet (endpoint ++ "statuses/user_timeline.json") (mkUserParam q)
deriveHasParamInstances ''StatusesUserTimeline
    [ "count"
    , "since_id"
    , "max_id"
    , "trim_user"
    , "exclude_replies"
    , "contributor_details"
    , "include_rts"
    ]

data StatusesHomeTimeline
homeTimeline :: APIRequest StatusesHomeTimeline [Status]
homeTimeline = APIRequestGet (endpoint ++ "statuses/home_timeline.json") def
deriveHasParamInstances ''StatusesHomeTimeline
    [ "count"
    , "since_id"
    , "max_id"
    , "trim_user"
    , "exclude_replies"
    , "contributor_details"
    , "include_entities"
    ]

data StatusesRetweetsOfMe
retweetsOfMe :: APIRequest StatusesRetweetsOfMe [Status]
retweetsOfMe = APIRequestGet (endpoint ++ "statuses/retweets_of_me.json") def
deriveHasParamInstances ''StatusesRetweetsOfMe
    [ "count"
    , "since_id"
    , "max_id"
    , "trim_user"
    , "include_entities"
    , "include_user_entities"
    ]

-- * Tweets

data StatusesRetweetsId
retweetsId :: StatusId -> APIRequest StatusesRetweetsId [RetweetedStatus]
retweetsId status_id = APIRequestGet uri def
  where uri = endpoint ++ "statuses/retweets/" ++ show status_id ++ ".json"
deriveHasParamInstances ''StatusesRetweetsId
    [ "count"
    , "trim_user"
    ]

data StatusesShowId
showId :: StatusId -> APIRequest StatusesShowId Status
showId status_id = APIRequestGet uri def
  where uri = endpoint ++ "statuses/show/" ++ show status_id ++ ".json"
deriveHasParamInstances ''StatusesShowId
    [ "trim_user"
    , "include_my_retweet"
    , "include_entities"
    ]

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
