module Web.Twitter.Conduit.Status (
    -- * Notice
    -- $notice

    -- * Timelines
    mentionsTimeline,
    userTimeline,
    homeTimeline,
    retweetsOfMe,

    -- * Tweets
    retweetsId,
    showId,
    destroyId,
    update,
    retweetId,
    updateWithMedia,
    lookup,
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
{-# DEPRECATED mentionsTimeline "Please use Web.Twitter.Conduit.API.statusesMentionsTimeline" #-}

userTimeline :: UserParam -> APIRequest StatusesUserTimeline [Status]
userTimeline = statusesUserTimeline
{-# DEPRECATED userTimeline "Please use Web.Twitter.Conduit.API.statusesUserTimeline" #-}

homeTimeline :: APIRequest StatusesHomeTimeline [Status]
homeTimeline = statusesHomeTimeline
{-# DEPRECATED homeTimeline "Please use Web.Twitter.Conduit.API.statusesHomeTimeline" #-}

retweetsOfMe :: APIRequest StatusesRetweetsOfMe [Status]
retweetsOfMe = statusesRetweetsOfMe
{-# DEPRECATED retweetsOfMe "Please use Web.Twitter.Conduit.API.statusesRetweetsOfMe" #-}

retweetsId :: StatusId -> APIRequest StatusesRetweetsId [RetweetedStatus]
retweetsId = statusesRetweetsId
{-# DEPRECATED retweetsId "Please use Web.Twitter.Conduit.API.statusesRetweetsId" #-}

showId :: StatusId -> APIRequest StatusesShowId Status
showId = statusesShowId
{-# DEPRECATED showId "Please use Web.Twitter.Conduit.API.statusesShowId" #-}

destroyId :: StatusId -> APIRequest StatusesDestroyId Status
destroyId = statusesDestroyId
{-# DEPRECATED destroyId "Please use Web.Twitter.Conduit.API.statusesDestroyId" #-}

update :: Text -> APIRequest StatusesUpdate Status
update = statusesUpdate
{-# DEPRECATED update "Please use Web.Twitter.Conduit.API.statusesUpdate" #-}

retweetId :: StatusId -> APIRequest StatusesRetweetId RetweetedStatus
retweetId = statusesRetweetId
{-# DEPRECATED retweetId "Please use Web.Twitter.Conduit.API.statusesRetweetId" #-}

updateWithMedia :: Text -> MediaData -> APIRequest StatusesUpdateWithMedia Status
updateWithMedia = statusesUpdateWithMedia
{-# DEPRECATED updateWithMedia "Please use Web.Twitter.Conduit.API.statusesUpdateWithMedia" #-}

lookup :: [StatusId] -> APIRequest StatusesLookup [Status]
lookup = statusesLookup
{-# DEPRECATED lookup "Please use Web.Twitter.Conduit.API.statusesLookup" #-}
