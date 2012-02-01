{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Stream (
  -- * StreamingAPI
  userstream,
  statusesFilter,
  ) where

import Web.Twitter.Api
import Web.Twitter.Monad
import Web.Twitter.Types
import Web.Twitter.Utils

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
