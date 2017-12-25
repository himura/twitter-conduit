module Web.Twitter.Conduit.Status
       (
       -- * Notice
       -- $notice

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
       , updateWithMedia
       , lookup
       ) where

import Data.Text (Text)
import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Request
import Web.Twitter.Types

import Prelude hiding (lookup)

-- $notice
--
-- This module provides aliases of statuses API, for backward compatibility.

mentionsTimeline :: APIRequest StatusesMentionsTimeline [Status]
mentionsTimeline = statusesMentionsTimeline

userTimeline :: UserParam -> APIRequest StatusesUserTimeline [Status]
userTimeline = statusesUserTimeline

homeTimeline :: APIRequest StatusesHomeTimeline [Status]
homeTimeline = statusesHomeTimeline

retweetsOfMe :: APIRequest StatusesRetweetsOfMe [Status]
retweetsOfMe = statusesRetweetsOfMe

retweetsId :: StatusId -> APIRequest StatusesRetweetsId [RetweetedStatus]
retweetsId = statusesRetweetsId

showId :: StatusId -> APIRequest StatusesShowId Status
showId = statusesShowId

destroyId :: StatusId -> APIRequest StatusesDestroyId Status
destroyId = statusesDestroyId

update :: Text -> APIRequest StatusesUpdate Status
update = statusesUpdate

retweetId :: StatusId -> APIRequest StatusesRetweetId RetweetedStatus
retweetId = statusesRetweetId

updateWithMedia :: Text -> MediaData -> APIRequest StatusesUpdateWithMedia Status
updateWithMedia = statusesUpdateWithMedia

lookup :: [StatusId] -> APIRequest StatusesLookup [Status]
lookup = statusesLookup
