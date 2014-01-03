{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Post
       ( statusesUpdate
       , statusesRetweetId

       , MediaData(..)
       , statusesUpdateWithMedia
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
import Web.Twitter.Conduit.Utils
import Network.HTTP.Client.MultipartFormData

import Network.HTTP.Conduit
import Data.Conduit
import Control.Arrow

import Data.Text (Text)
import Data.Text.Encoding as T

statusesUpdate :: TwitterBaseM m => Text -> HT.SimpleQuery -> TW m Status
statusesUpdate tweet query = apiPost "statuses/update.json" (("status", T.encodeUtf8 tweet):query)

favoritesCreate :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
favoritesCreate sid query = apiPost "favorites/create.json" (("id", showBS sid):query)

favoritesDestroy :: TwitterBaseM m => StatusId -> HT.SimpleQuery -> TW m Status
favoritesDestroy sid query = apiPost "favorites/destroy.json" (("id", showBS sid):query)

statusesRetweetId :: TwitterBaseM m => Integer -> HT.SimpleQuery -> TW m RetweetedStatus
statusesRetweetId tweetId = apiPost ("statuses/retweet/" ++ show tweetId ++ ".json")

friendshipsCreate :: TwitterBaseM m => UserParam -> HT.SimpleQuery -> TW m User
friendshipsCreate user query = apiPost "friendships/create.json" q
  where q = mkUserParam user ++ query

data MediaData = MediaFromFile FilePath
               | MediaRequestBody FilePath RequestBody

statusesUpdateWithMedia :: TwitterBaseM m
                        => Text
                        -> MediaData
                        -> HT.SimpleQuery
                        -> TW m Status
statusesUpdateWithMedia tweet mediaData query = do
    req <- formDataBody body =<< makeRequest "POST" (endpoint ++ "statuses/update_with_media.json") []
    res <- apiRequest req
    res $$+- sinkFromJSON
  where
    body = mediaBody mediaData : partQuery

    partQuery = map (uncurry partBS . first T.decodeUtf8) $ ("status", T.encodeUtf8 tweet) : query
    mediaBody (MediaFromFile fp) = partFileSource "media[]" fp
    mediaBody (MediaRequestBody filename filebody) = partFileRequestBody "media[]" filename filebody
