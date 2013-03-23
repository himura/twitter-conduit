{-# LANGUAGE OverloadedStrings #-}

module Secret
       ( tokens
       ) where

import Web.Authenticate.OAuth

tokens :: OAuth
tokens = def { oauthServerName = "twitter"
             , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
             , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
             , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
             , oauthConsumerKey = error "You MUST specify oauthConsumerKey parameter."
             , oauthConsumerSecret = error "You MUST specify oauthConsumerSecret parameter."
             , oauthSignatureMethod = HMACSHA1
             , oauthCallback = Nothing
             }
