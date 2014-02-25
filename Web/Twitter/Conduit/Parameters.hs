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
       ) where

import Web.Twitter.Conduit.Parameters.Internal
import Web.Twitter.Conduit.Parameters.TH

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
