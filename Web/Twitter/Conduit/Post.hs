{-# LANGUAGE OverloadedStrings #-}

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

import Data.Text (Text)
import Data.Text.Encoding as T

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL


statusesUpdate :: Text -> HT.Query -> TW ()
statusesUpdate tweet query = api "POST" (endpoint ++ "statuses/update.json") (("status", Just $ T.encodeUtf8 tweet):query) C.$$ CL.sinkNull

favoritesCreate :: StatusId -> HT.Query -> TW Status
favoritesCreate sid query = api "POST" (endpoint ++ "favorites/create/" ++ show sid ++ ".json") query C.$$ sinkFromJSON

favoritesDestroy :: StatusId -> HT.Query -> TW Status
favoritesDestroy sid query = api "POST" (endpoint ++ "favorites/destroy/" ++ show sid ++ ".json") query C.$$ sinkFromJSON

statusesRetweetId :: Integer -> HT.Query -> TW RetweetedStatus
statusesRetweetId tweetId query = api "POST" (endpoint ++ "statuses/retweet/" ++ show tweetId ++ ".json") query C.$$ sinkFromJSON

friendshipsCreate :: UserParam -> HT.Query -> TW User
friendshipsCreate user query = api "POST" (endpoint ++ "friendships/create.json") q C.$$ sinkFromJSON
  where q = mkUserParam user ++ query
