{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Post
       ( statusesUpdate
         , retweet

         -- * Friendship
         , friendshipCreate
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
import Web.Twitter.Enumerator.Types (UserId)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Data.Enumerator (Iteratee)

statusesUpdate :: ByteString -> Iteratee ByteString IO a -> Iteratee ByteString TW a
statusesUpdate tweet iter = api True "POST" (endpoint ++ "statuses/update.json") [("status", Just tweet)] iter

retweet :: Integer -> Iteratee ByteString IO a -> Iteratee ByteString TW a
retweet id iter = api True "POST" (endpoint ++ "statuses/retweet/" ++ (show id) ++ ".json") [] iter

friendshipCreate :: UserId -> Iteratee ByteString IO a -> Iteratee ByteString TW a
friendshipCreate uid iter = api True "POST" (endpoint ++ "friendships/create.json") [("user_id", Just $ B8.pack $ show uid)] iter
