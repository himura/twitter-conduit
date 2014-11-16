{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}

module Web.Twitter.Conduit.Status
       (
       -- * Timelines
         StatusesMentionsTimeline
       , mentionsTimeline
       , StatusesUserTimeline
       , userTimeline
       , StatusesHomeTimeline
       , homeTimeline
       , StatusesRetweetsOfMe
       , retweetsOfMe
       -- * Tweets
       , StatusesRetweetsId
       , retweetsId
       , StatusesShowId
       , showId
       , StatusesDestroyId
       , destroyId
       , StatusesUpdate
       , update
       , StatusesRetweetId
       , retweetId
       , MediaData (..)
       , StatusesUpdateWithMedia
       , updateWithMedia
       -- , oembed
       -- , retweetersIds
       , StatusesLookup
       , lookup
       ) where

import Prelude hiding ( lookup )
import Web.Twitter.Conduit.Base
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Parameters.TH
import Web.Twitter.Types

import qualified Data.Text as T
import Network.HTTP.Client.MultipartFormData
import Data.Default

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens

-- * Timelines

data StatusesMentionsTimeline
-- | Returns query data asks the most recent mentions for the authenticating user.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' 'mentionsTimeline'
-- @
--
-- >>> mentionsTimeline
-- APIRequestGet "https://api.twitter.com/1.1/statuses/mentions_timeline.json" []
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
-- | Returns query data asks a collection of the most recent Tweets posted by the user indicated by the screen_name or user_id parameters.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' $ 'userTimeline' ('ScreenNameParam' \"thimura\")
-- @
--
-- >>> userTimeline (ScreenNameParam "thimura")
-- APIRequestGet "https://api.twitter.com/1.1/statuses/user_timeline.json" [("screen_name","thimura")]
-- >>> userTimeline (ScreenNameParam "thimura") & includeRts ?~ True & count ?~ 200
-- APIRequestGet "https://api.twitter.com/1.1/statuses/user_timeline.json" [("count","200"),("include_rts","true"),("screen_name","thimura")]
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
-- | Returns query data asks a collection of the most recentTweets and retweets posted by the authenticating user and the users they follow.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' 'homeTimeline'
-- @
--
-- >>> homeTimeline
-- APIRequestGet "https://api.twitter.com/1.1/statuses/home_timeline.json" []
-- >>> homeTimeline & count ?~ 200
-- APIRequestGet "https://api.twitter.com/1.1/statuses/home_timeline.json" [("count","200")]
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
-- | Returns query data asks the most recent tweets authored by the authenticating user that have been retweeted by others.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' 'retweetsOfMe'
-- @
--
-- >>> retweetsOfMe
-- APIRequestGet "https://api.twitter.com/1.1/statuses/retweets_of_me.json" []
-- >>> retweetsOfMe & count ?~ 100
-- APIRequestGet "https://api.twitter.com/1.1/statuses/retweets_of_me.json" [("count","100")]
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
-- | Returns query data asks the most recent tweets authored by the authenticating user that have been retweeted by others.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'retweetsId' 1234567890
-- @
--
-- >>> retweetsId 1234567890
-- APIRequestGet "https://api.twitter.com/1.1/statuses/retweets/1234567890.json" []
-- >>> retweetsId 1234567890 & count ?~ 100
-- APIRequestGet "https://api.twitter.com/1.1/statuses/retweets/1234567890.json" [("count","100")]
retweetsId :: StatusId -> APIRequest StatusesRetweetsId [RetweetedStatus]
retweetsId status_id = APIRequestGet uri def
  where uri = endpoint ++ "statuses/retweets/" ++ show status_id ++ ".json"
deriveHasParamInstances ''StatusesRetweetsId
    [ "count"
    , "trim_user"
    ]

data StatusesShowId
-- | Returns query data asks a single Tweet, specified by the id parameter.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'showId' 1234567890
-- @
--
-- >>> showId 1234567890
-- APIRequestGet "https://api.twitter.com/1.1/statuses/show/1234567890.json" []
-- >>> showId 1234567890 & includeMyRetweet ?~ True
-- APIRequestGet "https://api.twitter.com/1.1/statuses/show/1234567890.json" [("include_my_retweet","true")]
showId :: StatusId -> APIRequest StatusesShowId Status
showId status_id = APIRequestGet uri def
  where uri = endpoint ++ "statuses/show/" ++ show status_id ++ ".json"
deriveHasParamInstances ''StatusesShowId
    [ "trim_user"
    , "include_my_retweet"
    , "include_entities"
    ]

data StatusesDestroyId
-- | Returns post data which destroys the status specified by the require ID parameter.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'destroyId' 1234567890
-- @
--
-- >>> destroyId 1234567890
-- APIRequestPost "https://api.twitter.com/1.1/statuses/destroy/1234567890.json" []
destroyId :: StatusId -> APIRequest StatusesDestroyId Status
destroyId status_id = APIRequestPost uri def
  where uri = endpoint ++ "statuses/destroy/" ++ show status_id ++ ".json"
deriveHasParamInstances ''StatusesDestroyId
    [ "trim_user"
    ]

data StatusesUpdate
-- | Returns post data which updates the authenticating user's current status.
-- To upload an image to accompany the tweet, use 'updateWithMedia'.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'update' \"Hello World\"
-- @
--
-- >>> update "Hello World"
-- APIRequestPost "https://api.twitter.com/1.1/statuses/update.json" [("status","Hello World")]
-- >>> update "Hello World" & inReplyToStatusId ?~ 1234567890
-- APIRequestPost "https://api.twitter.com/1.1/statuses/update.json" [("in_reply_to_status_id","1234567890"),("status","Hello World")]
update :: T.Text -> APIRequest StatusesUpdate Status
update status = APIRequestPost uri [("status", PVString status)]
  where uri = endpoint ++ "statuses/update.json"
deriveHasParamInstances ''StatusesUpdate
    [ "in_reply_to_status_id"
    -- , "lat_long"
    -- , "place_id"
    , "display_coordinates"
    , "trim_user"
    , "media_ids"
    ]

data StatusesRetweetId
-- | Returns post data which retweets a tweet, specified by ID.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'retweetId' 1234567890
-- @
--
-- >>> retweetId 1234567890
-- APIRequestPost "https://api.twitter.com/1.1/statuses/retweet/1234567890.json" []
retweetId :: StatusId -> APIRequest StatusesRetweetId RetweetedStatus
retweetId status_id = APIRequestPost uri def
  where uri = endpoint ++ "statuses/retweet/" ++ show status_id ++ ".json"
deriveHasParamInstances ''StatusesRetweetId
    [ "trim_user"
    ]

data StatusesUpdateWithMedia
-- | Returns post data which updates the authenticating user's current status and attaches media for upload.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'updateWithMedia' \"Hello World\" ('MediaFromFile' \"/home/thimura/test.jpeg\")
-- @
--
-- >>> updateWithMedia "Hello World" (MediaFromFile "/home/fuga/test.jpeg")
-- APIRequestPostMultipart "https://api.twitter.com/1.1/statuses/update_with_media.json" [("status","Hello World")]
updateWithMedia :: T.Text
                -> MediaData
                -> APIRequest StatusesUpdateWithMedia Status
updateWithMedia tweet mediaData =
    APIRequestPostMultipart uri [("status", PVString tweet)] [mediaBody mediaData]
  where
    uri = endpoint ++ "statuses/update_with_media.json"
    mediaBody (MediaFromFile fp) = partFileSource "media[]" fp
    mediaBody (MediaRequestBody filename filebody) = partFileRequestBody "media[]" filename filebody
deriveHasParamInstances ''StatusesUpdateWithMedia
    [ "possibly_sensitive"
    , "in_reply_to_status_id"
    -- , "lat_long"
    -- , "place_id"
    , "display_coordinates"
    ]

data StatusesLookup
-- | Returns fully-hydrated tweet objects for up to 100 tweets per request, as specified by comma-separated values passed to the id parameter.
--
-- You can perform a request using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'lookup' [20, 432656548536401920]
-- @
--
-- >>> lookup [10]
-- APIRequestGet "https://api.twitter.com/1.1/statuses/lookup.json" [("id","10")]
-- >>> lookup [10, 432656548536401920]
-- APIRequestGet "https://api.twitter.com/1.1/statuses/lookup.json" [("id","10,432656548536401920")]
-- >>> lookup [10, 432656548536401920] & includeEntities ?~ True
-- APIRequestGet "https://api.twitter.com/1.1/statuses/lookup.json" [("include_entities","true"),("id","10,432656548536401920")]
lookup :: [StatusId] -> APIRequest StatusesLookup [Status]
lookup ids = APIRequestGet (endpoint ++ "statuses/lookup.json") [("id", PVIntegerArray ids)]
deriveHasParamInstances ''StatusesLookup
    [ "include_entities"
    , "trim_user"
    , "map"
    ]
