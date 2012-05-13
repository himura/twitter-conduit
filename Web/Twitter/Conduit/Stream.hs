{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Conduit.Stream (
  -- * StreamingAPI
  userstream,
  statusesFilter,
  ) where

import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Types
import Web.Twitter.Conduit.Utils

import qualified Data.Conduit as C
import qualified Network.HTTP.Types as HT

userstream :: C.Source TW StreamingAPI
userstream =
  api "GET" "https://userstream.twitter.com/2/user.json" []
    C.$= conduitFromJSON

statusesFilter :: HT.Query -> C.Source TW StreamingAPI
statusesFilter query =
  api "GET" "https://stream.twitter.com/1/statuses/filter.json" query
    C.$= conduitFromJSON
