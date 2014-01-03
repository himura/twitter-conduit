{-# LANGUAGE OverloadedStrings #-}

module Secret
       ( tokens
       ) where

import Web.Authenticate.OAuth
import Web.Twitter.Conduit

tokens :: OAuth
tokens = twitterOAuth
  { oauthConsumerKey = error "You MUST specify oauthConsumerKey parameter."
  , oauthConsumerSecret = error "You MUST specify oauthConsumerSecret parameter."
  }
