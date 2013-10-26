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
import Web.Twitter.Types
import Web.Twitter.Conduit.Param

import Data.Text (Text)
import Data.Text.Encoding as T

statusesUpdate :: TwitterBaseM m => Text -> HT.SimpleQuery -> TW m ()
statusesUpdate tweet query = apiPost signOAuthTW "statuses/update.json" (("status", T.encodeUtf8 tweet):query)

favoritesCreate :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
favoritesCreate sid query = apiPost signOAuthTW ("favorites/create/" ++ show sid ++ ".json") query

favoritesDestroy :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
favoritesDestroy sid query = apiPost signOAuthTW ("favorites/destroy/" ++ show sid ++ ".json") query

statusesRetweetId :: TwitterBaseM m => Integer -> HT.SimpleQuery -> TW m RetweetedStatus
statusesRetweetId tweetId query = apiPost signOAuthTW ("statuses/retweet/" ++ show tweetId ++ ".json") query

friendshipsCreate :: TwitterBaseM m => UserParam -> HT.SimpleQuery -> TW m User
friendshipsCreate user query = apiPost signOAuthTW "friendships/create.json" q
  where q = mkUserParam user ++ query
