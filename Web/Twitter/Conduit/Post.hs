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
import Web.Twitter.Conduit.Param

import Data.Text (Text)
import Data.Text.Encoding as T

statusesUpdate :: TwitterBaseM m => Text -> HT.Query -> TW WithToken m ()
statusesUpdate tweet query = apiPost authRequired "statuses/update.json" (("status", Just $ T.encodeUtf8 tweet):query)

favoritesCreate :: TwitterBaseM m => StatusId -> HT.Query -> TW WithToken m Status
favoritesCreate sid query = apiPost authRequired ("favorites/create/" ++ show sid ++ ".json") query

favoritesDestroy :: TwitterBaseM m => StatusId -> HT.Query -> TW WithToken m Status
favoritesDestroy sid query = apiPost authRequired ("favorites/destroy/" ++ show sid ++ ".json") query

statusesRetweetId :: TwitterBaseM m => Integer -> HT.Query -> TW WithToken m RetweetedStatus
statusesRetweetId tweetId query = apiPost authRequired ("statuses/retweet/" ++ show tweetId ++ ".json") query

friendshipsCreate :: TwitterBaseM m => UserParam -> HT.Query -> TW WithToken m User
friendshipsCreate user query = apiPost authRequired "friendships/create.json" q
  where q = mkUserParam user ++ query
