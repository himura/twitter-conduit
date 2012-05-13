{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Conduit.Post
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

import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Api

import Data.ByteString (ByteString)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

statusesUpdate :: ByteString -> TW ()
statusesUpdate tweet =
    api "POST" "statuses/update.json" [("status", Just tweet)] C.$$ CL.sinkNull
