{-# LANGUAGE OverloadedStrings #-}

module Secret
       ( tokens
       ) where

import Web.Authenticate.OAuth

tokens :: OAuth
tokens = OAuth { oauthServerName = "twitter"
               , oauthRequestUri = "http://twitter.com/oauth/request_token"
               , oauthAccessTokenUri = "http://twitter.com/oauth/access_token"
               , oauthAuthorizeUri = "http://twitter.com/oauth/authorize"
               , oauthConsumerKey = "<consumer key>"
               , oauthConsumerSecret = "<consumer secret>"
               , oauthSignatureMethod = HMACSHA1
               , oauthCallback = Nothing
               }
