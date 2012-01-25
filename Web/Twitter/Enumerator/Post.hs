{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Post
       ( statusesUpdate

         -- * Friendship
         -- , friendshipCreate
         -- , friendshipDestroy

         -- * Favorites
         -- , favoritesCreate
         -- , favoritesDestroy

         -- * Lists
         -- , listsCreate
         -- , listsDestroy
         -- , listsUpdate
         -- , listsMembersCreate
         -- , listsMembersDestroy
       )
       where

import Web.Twitter.Enumerator.Monad
import Web.Twitter.Enumerator.Api

import Data.ByteString (ByteString)

import Data.Enumerator (Iteratee)

statusesUpdate :: ByteString -> Iteratee ByteString IO a -> Iteratee ByteString TW a
statusesUpdate tweet iter = api "POST" (endpoint ++ "statuses/update.json") [("status", Just tweet)] iter
