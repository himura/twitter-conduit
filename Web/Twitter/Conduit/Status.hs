{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

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
import Web.Twitter.Conduit.Request.Internal
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Types

import qualified Data.Text as T
import Network.HTTP.Client.MultipartFormData
import Data.Default

-- $setup
-- >>> :set -XOverloadedStrings -XOverloadedLabels
-- >>> import Control.Lens

-- * Timelines

-- | Returns query data asks the most recent mentions for the authenticating user.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' 'mentionsTimeline'
-- @
--
-- >>> mentionsTimeline
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/mentions_timeline.json" []
mentionsTimeline :: APIRequest StatusesMentionsTimeline [Status]
mentionsTimeline = APIRequest "GET" (endpoint ++ "statuses/mentions_timeline.json") def
type StatusesMentionsTimeline = '[
      "count" ':= Integer
    , "since_id" ':= Integer
    , "max_id" ':= Integer
    , "trim_user" ':= Bool
    , "contributor_details" ':= Bool
    , "include_entities" ':= Bool
    , "tweet_mode" ':= T.Text
    ]

-- | Returns query data asks a collection of the most recent Tweets posted by the user indicated by the screen_name or user_id parameters.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' $ 'userTimeline' ('ScreenNameParam' \"thimura\")
-- @
--
-- >>> userTimeline (ScreenNameParam "thimura")
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/user_timeline.json" [("screen_name","thimura")]
-- >>> userTimeline (ScreenNameParam "thimura") & #include_rts ?~ True & #count ?~ 200
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/user_timeline.json" [("count","200"),("include_rts","true"),("screen_name","thimura")]
userTimeline :: UserParam -> APIRequest StatusesUserTimeline [Status]
userTimeline q = APIRequest "GET" (endpoint ++ "statuses/user_timeline.json") (mkUserParam q)
type StatusesUserTimeline = '[
      "count" ':= Integer
    , "since_id" ':= Integer
    , "max_id" ':= Integer
    , "trim_user" ':= Bool
    , "exclude_replies" ':= Bool
    , "contributor_details" ':= Bool
    , "include_rts" ':= Bool
    , "tweet_mode" ':= T.Text
    ]

-- | Returns query data asks a collection of the most recentTweets and retweets posted by the authenticating user and the users they follow.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' 'homeTimeline'
-- @
--
-- >>> homeTimeline
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/home_timeline.json" []
-- >>> homeTimeline & #count ?~ 200
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/home_timeline.json" [("count","200")]
homeTimeline :: APIRequest StatusesHomeTimeline [Status]
homeTimeline = APIRequest "GET" (endpoint ++ "statuses/home_timeline.json") def
type StatusesHomeTimeline = '[
      "count" ':= Integer
    , "since_id" ':= Integer
    , "max_id" ':= Integer
    , "trim_user" ':= Bool
    , "exclude_replies" ':= Bool
    , "contributor_details" ':= Bool
    , "include_entities" ':= Bool
    , "tweet_mode" ':= T.Text
    ]

-- | Returns query data asks the most recent tweets authored by the authenticating user that have been retweeted by others.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' 'retweetsOfMe'
-- @
--
-- >>> retweetsOfMe
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/retweets_of_me.json" []
-- >>> retweetsOfMe & #count ?~ 100
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/retweets_of_me.json" [("count","100")]
retweetsOfMe :: APIRequest StatusesRetweetsOfMe [Status]
retweetsOfMe = APIRequest "GET" (endpoint ++ "statuses/retweets_of_me.json") def
type StatusesRetweetsOfMe = '[
      "count" ':= Integer
    , "since_id" ':= Integer
    , "max_id" ':= Integer
    , "trim_user" ':= Bool
    , "include_entities" ':= Bool
    , "include_user_entities" ':= Bool
    , "tweet_mode" ':= T.Text
    ]

-- * Tweets

-- | Returns query data that asks for the most recent retweets of the specified tweet
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'retweetsId' 1234567890
-- @
--
-- >>> retweetsId 1234567890
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/retweets/1234567890.json" []
-- >>> retweetsId 1234567890 & #count ?~ 100
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/retweets/1234567890.json" [("count","100")]
retweetsId :: StatusId -> APIRequest StatusesRetweetsId [RetweetedStatus]
retweetsId status_id = APIRequest "GET" uri def
  where uri = endpoint ++ "statuses/retweets/" ++ show status_id ++ ".json"
type StatusesRetweetsId = '[
      "count" ':= Integer
    , "trim_user" ':= Bool
    ]

-- | Returns query data asks a single Tweet, specified by the id parameter.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'showId' 1234567890
-- @
--
-- >>> showId 1234567890
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/show/1234567890.json" []
-- >>> showId 1234567890 & #include_my_retweet ?~ True
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/show/1234567890.json" [("include_my_retweet","true")]
showId :: StatusId -> APIRequest StatusesShowId Status
showId status_id = APIRequest "GET" uri def
  where uri = endpoint ++ "statuses/show/" ++ show status_id ++ ".json"
type StatusesShowId = '[
      "trim_user" ':= Bool
    , "include_my_retweet" ':= Bool
    , "include_entities" ':= Bool
    , "include_ext_alt_text" ':= Bool
    , "tweet_mode" ':= T.Text
    ]

-- | Returns post data which destroys the status specified by the require ID parameter.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'destroyId' 1234567890
-- @
--
-- >>> destroyId 1234567890
-- APIRequest "POST" "https://api.twitter.com/1.1/statuses/destroy/1234567890.json" []
destroyId :: StatusId -> APIRequest StatusesDestroyId Status
destroyId status_id = APIRequest "POST" uri def
  where uri = endpoint ++ "statuses/destroy/" ++ show status_id ++ ".json"
type StatusesDestroyId = '[
      "trim_user" ':= Bool
    , "tweet_mode" ':= T.Text
    ]

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
-- APIRequest "POST" "https://api.twitter.com/1.1/statuses/update.json" [("status","Hello World")]
-- >>> update "Hello World" & #in_reply_to_status_id ?~ 1234567890
-- APIRequest "POST" "https://api.twitter.com/1.1/statuses/update.json" [("in_reply_to_status_id","1234567890"),("status","Hello World")]
update :: T.Text -> APIRequest StatusesUpdate Status
update status = APIRequest "POST" uri [("status", PVString status)]
  where uri = endpoint ++ "statuses/update.json"
type StatusesUpdate = '[
      "in_reply_to_status_id" ':= Integer
    -- , "lat_long"
    -- , "place_id"
    , "display_coordinates" ':= Bool
    , "trim_user" ':= Bool
    , "media_ids" ':= [Integer]
    , "tweet_mode" ':= T.Text
    ]

-- | Returns post data which retweets a tweet, specified by ID.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'retweetId' 1234567890
-- @
--
-- >>> retweetId 1234567890
-- APIRequest "POST" "https://api.twitter.com/1.1/statuses/retweet/1234567890.json" []
retweetId :: StatusId -> APIRequest StatusesRetweetId RetweetedStatus
retweetId status_id = APIRequest "POST" uri def
  where uri = endpoint ++ "statuses/retweet/" ++ show status_id ++ ".json"
type StatusesRetweetId = '[
      "trim_user" ':= Bool
    ]

-- | Returns post data which updates the authenticating user's current status and attaches media for upload.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'updateWithMedia' \"Hello World\" ('MediaFromFile' \"/home/thimura/test.jpeg\")
-- @
--
-- >>> updateWithMedia "Hello World" (MediaFromFile "/home/fuga/test.jpeg")
-- APIRequestMultipart "POST" "https://api.twitter.com/1.1/statuses/update_with_media.json" [("status","Hello World")]
updateWithMedia :: T.Text
                -> MediaData
                -> APIRequest StatusesUpdateWithMedia Status
updateWithMedia tweet mediaData =
    APIRequestMultipart "POST" uri [("status", PVString tweet)] [mediaBody mediaData]
  where
    uri = endpoint ++ "statuses/update_with_media.json"
    mediaBody (MediaFromFile fp) = partFileSource "media[]" fp
    mediaBody (MediaRequestBody filename filebody) = partFileRequestBody "media[]" filename filebody
type StatusesUpdateWithMedia = '[
      "possibly_sensitive" ':= Bool
    , "in_reply_to_status_id" ':= Integer
    -- , "lat_long"
    -- , "place_id"
    , "display_coordinates" ':= Bool
    , "tweet_mode" ':= T.Text
    ]

-- | Returns fully-hydrated tweet objects for up to 100 tweets per request, as specified by comma-separated values passed to the id parameter.
--
-- You can perform a request using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'lookup' [20, 432656548536401920]
-- @
--
-- >>> lookup [10]
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/lookup.json" [("id","10")]
-- >>> lookup [10, 432656548536401920]
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/lookup.json" [("id","10,432656548536401920")]
-- >>> lookup [10, 432656548536401920] & #include_entities ?~ True
-- APIRequest "GET" "https://api.twitter.com/1.1/statuses/lookup.json" [("include_entities","true"),("id","10,432656548536401920")]
lookup :: [StatusId] -> APIRequest StatusesLookup [Status]
lookup ids = APIRequest "GET" (endpoint ++ "statuses/lookup.json") [("id", PVIntegerArray ids)]
type StatusesLookup = '[
      "include_entities" ':= Bool
    , "trim_user" ':= Bool
    , "map" ':= Bool
    , "tweet_mode" ':= T.Text
    ]
