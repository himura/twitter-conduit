{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Twitter.Conduit.Parameters
       ( Parameters (..)
       , HasSinceIdParam (..)
       , HasCountParam (..)
       , HasMaxIdParam (..)
       , HasCursorParam (..)
       , HasTrimUserParam (..)
       , HasExcludeRepliesParam (..)
       , HasContributorDetailsParam (..)
       , HasIncludeEntitiesParam (..)
       , HasIncludeUserEntitiesParam (..)
       , HasIncludeRtsParam (..)
       , HasIncludeMyRetweetParam (..)
       , HasInReplyToStatusIdParam (..)
       , HasDisplayCoordinatesParam (..)
       , HasPossiblySensitiveParam (..)
       , HasLangParam (..)
       , HasLocaleParam (..)
       , HasUntilParam (..)
       , HasSkipStatusParam (..)
       , HasFollowParam (..)
       ) where

import Web.Twitter.Conduit.Parameters.Internal
import Web.Twitter.Conduit.Parameters.TH
import Web.Twitter.Types
import Data.Time.Calendar (Day)
import Data.Text.Strict.Lens
import Data.Text (Text)

defineHasParamClass "count" ''Integer 'readShow
defineHasParamClass "since_id" ''Integer 'readShow
defineHasParamClass "max_id" ''Integer 'readShow
defineHasParamClass "cursor" ''Integer 'readShow
defineHasParamClass "trim_user" ''Bool 'booleanQuery
defineHasParamClass "exclude_replies" ''Bool 'booleanQuery
defineHasParamClass "contributor_details" ''Bool 'booleanQuery
defineHasParamClass "include_entities" ''Bool 'booleanQuery
defineHasParamClass "include_user_entities" ''Bool 'booleanQuery
defineHasParamClass "include_rts" ''Bool 'booleanQuery
defineHasParamClass "include_my_retweet" ''Bool 'booleanQuery
defineHasParamClass "in_reply_to_status_id" ''StatusId 'readShow
defineHasParamClass "display_coordinates" ''Bool 'booleanQuery
defineHasParamClass "possibly_sensitive" ''Bool 'booleanQuery
defineHasParamClass "lang" ''Text 'utf8
defineHasParamClass "locale" ''Text 'utf8
defineHasParamClass "until" ''Day 'readShow
defineHasParamClass "skip_status" ''Bool 'booleanQuery
defineHasParamClass "follow" ''Bool 'booleanQuery
