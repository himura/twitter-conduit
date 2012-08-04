{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Stream
       (
       -- * StreamingAPI
         userstream
       , statusesFilter
  ) where

import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Types
import Web.Twitter.Conduit.Utils

import qualified Data.Conduit as C
import qualified Network.HTTP.Types as HT

userstream :: TwitterBaseM m => C.Source (TW WithToken m) StreamingAPI
userstream =
  api authRequired "GET" "https://userstream.twitter.com/2/user.json" []
    C.$= conduitFromJSON

statusesFilter :: TwitterBaseM m => HT.Query -> C.Source (TW WithToken m) StreamingAPI
statusesFilter query =
  api authRequired "GET" "https://stream.twitter.com/1/statuses/filter.json" query
    C.$= conduitFromJSON
