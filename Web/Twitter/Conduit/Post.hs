{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Post
       ( statusesUpdate
       , statusesRetweetId

       -- * Friends & Followers
       , friendshipsCreate
       -- , friendshipDestroy

       -- * Favorites
       , favoritesCreate
       , favoritesDestroy

       -- * Lists
       -- , listsCreate
       -- , listsDestroy
       -- , listsUpdate
       -- , listsMembersCreate
       -- , listsMembersDestroy
       )
       where

import qualified Network.HTTP.Types as HT
import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Types
import Web.Twitter.Conduit.Utils
import Web.Twitter.Conduit.Param

import Data.Text (Text)
import Data.Text.Encoding as T

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

statusesUpdate :: TwitterBaseM m => Text -> HT.Query -> TW WithToken m ()
statusesUpdate tweet query = api authRequired "POST" (endpoint ++ "statuses/update.json") (("status", Just $ T.encodeUtf8 tweet):query) C.$$ CL.sinkNull

favoritesCreate :: TwitterBaseM m => StatusId -> HT.Query -> TW WithToken m Status
favoritesCreate sid query = api authRequired "POST" (endpoint ++ "favorites/create/" ++ show sid ++ ".json") query C.$$ sinkFromJSON

favoritesDestroy :: TwitterBaseM m => StatusId -> HT.Query -> TW WithToken m Status
favoritesDestroy sid query = api authRequired "POST" (endpoint ++ "favorites/destroy/" ++ show sid ++ ".json") query C.$$ sinkFromJSON

statusesRetweetId :: TwitterBaseM m => Integer -> HT.Query -> TW WithToken m RetweetedStatus
statusesRetweetId tweetId query = api authRequired "POST" (endpoint ++ "statuses/retweet/" ++ show tweetId ++ ".json") query C.$$ sinkFromJSON

friendshipsCreate :: TwitterBaseM m => UserParam -> HT.Query -> TW WithToken m User
friendshipsCreate user query = api authRequired "POST" (endpoint ++ "friendships/create.json") q C.$$ sinkFromJSON
  where q = mkUserParam user ++ query
