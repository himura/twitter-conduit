{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Twitter.Conduit.Parameters
       ( Parameters (..)
       , HasSinceIdParam (..)
       , HasCountParam (..)
       , HasMaxIdParam (..)
       , HasCursorParam (..)
       ) where

import Web.Twitter.Conduit.Parameters.Internal
import Web.Twitter.Conduit.Parameters.TH

defineHasParamClass "count" ''Integer 'readShow
defineHasParamClass "since_id" ''Integer 'readShow
defineHasParamClass "max_id" ''Integer 'readShow
defineHasParamClass "cursor" ''Integer 'readShow
