{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Post
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

       -- * Deprecated
       , retweet
       , friendshipCreate

       ) where

import Data.Aeson hiding (Error)

import Web.Twitter.Enumerator.Types
import Web.Twitter.Enumerator.Monad
import Web.Twitter.Enumerator.Utils
import Web.Twitter.Enumerator.Api

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)

import qualified Network.HTTP.Types as HT
import Data.Enumerator (Iteratee, (=$), run_)
import qualified Data.Enumerator.List as EL

apiPost :: FromJSON a => String -> HT.Query -> Iteratee a IO b -> TW b
apiPost uri query iter = run_ $ api True "POST" uri query (handleParseError iter')
  where iter' = enumJSON =$ EL.map fromJSON' =$ skipNothing =$ iter

statusesUpdate :: Text -> HT.Query -> TW Status
statusesUpdate tweet query = apiPost (endpoint ++ "statuses/update.json") q (debugEE =$ EL.head_)
  where q = ("status", Just . T.encodeUtf8 $ tweet):query

favoritesCreate :: StatusId -> HT.Query -> TW Status
favoritesCreate sid query = apiPost (endpoint ++ "favorites/create/" ++ show sid ++ ".json") query EL.head_

favoritesDestroy :: StatusId -> HT.Query -> TW Status
favoritesDestroy sid query = apiPost (endpoint ++ "favorites/destroy/" ++ show sid ++ ".json") query EL.head_

statusesRetweetId :: Integer -> HT.Query -> TW RetweetedStatus
statusesRetweetId tweetId query = apiPost (endpoint ++ "statuses/retweet/" ++ show tweetId ++ ".json") query EL.head_

friendshipsCreate :: UserParam -> HT.Query -> TW User
friendshipsCreate user query = apiPost (endpoint ++ "friendships/create.json") q EL.head_
  where q = mkUserParam user ++ query

{-# DEPRECATED retweet "'retweet' will be removed in future releases. Use 'statusesRetweetId' instead" #-}
retweet :: Integer -> Iteratee ByteString IO a -> Iteratee ByteString TW a
retweet tweetId = api True "POST" (endpoint ++ "statuses/retweet/" ++ show tweetId ++ ".json") []

{-# DEPRECATED friendshipCreate "'friendshipCreate' will be removed in future releases. Use 'friendshipsCreate' instead" #-}
friendshipCreate :: UserId -> Iteratee ByteString IO a -> Iteratee ByteString TW a
friendshipCreate uid = api True "POST" (endpoint ++ "friendships/create.json") [("user_id", toMaybeByteString uid)]
