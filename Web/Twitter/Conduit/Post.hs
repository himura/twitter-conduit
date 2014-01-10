{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Post
       (
       -- * Friends & Followers
         friendshipsCreate
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
import Web.Twitter.Conduit.Utils

favoritesCreate :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
favoritesCreate sid query = apiPost "favorites/create.json" (("id", showBS sid):query)

favoritesDestroy :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
favoritesDestroy sid query = apiPost "favorites/destroy.json" (("id", showBS sid):query)

friendshipsCreate :: TwitterBaseM m => UserParam -> HT.SimpleQuery -> TW m User
friendshipsCreate user query = apiPost "friendships/create.json" q
  where q = mkUserParam user ++ query
