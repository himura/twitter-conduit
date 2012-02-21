{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Post
       ( statusesUpdate
       , retweet

       -- * Friendship
       , friendshipCreate
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
        ) where

import Data.Aeson hiding (Error)

import Web.Twitter.Enumerator.Types
import Web.Twitter.Enumerator.Monad
import Web.Twitter.Enumerator.Utils
import Web.Twitter.Enumerator.Api

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import qualified Network.HTTP.Types as HT
import Data.Enumerator (Iteratee, (=$), run_)
import qualified Data.Enumerator.List as EL

apiPost :: FromJSON a => String -> HT.Query -> Iteratee a IO b -> TW b
apiPost uri query iter = run_ $ api AuthRequired "POST" uri query (handleParseError iter')
  where iter' = enumJSON =$ EL.map fromJSON' =$ skipNothing =$ iter

statusesUpdate :: ByteString -> Iteratee ByteString IO a -> Iteratee ByteString TW a
statusesUpdate tweet iter = api AuthRequired "POST" (endpoint ++ "statuses/update.json") [("status", Just tweet)] iter

favoritesCreate :: StatusId -> HT.Query -> TW Status
favoritesCreate sid query = apiPost (endpoint ++ "favorites/create/" ++ show sid ++ ".json") query EL.head_

favoritesDestroy :: StatusId -> HT.Query -> TW Status
favoritesDestroy sid query = apiPost (endpoint ++ "favorites/destroy/" ++ show sid ++ ".json") query EL.head_

retweet :: Integer -> Iteratee ByteString IO a -> Iteratee ByteString TW a
retweet tweetId iter = api AuthRequired "POST" (endpoint ++ "statuses/retweet/" ++ (show tweetId) ++ ".json") [] iter

friendshipCreate :: UserId -> Iteratee ByteString IO a -> Iteratee ByteString TW a
friendshipCreate uid iter = api AuthRequired "POST" (endpoint ++ "friendships/create.json") [("user_id", Just $ B8.pack $ show uid)] iter
