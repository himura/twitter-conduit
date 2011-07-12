{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Fetch
       ( api
       , statusesPublicTimeline
       , statusesUserTimeline
       , statusesFriendsTimeline
       , statusesReplies
       , statusesMentions
       , statusesRetweetedByMe
       , statusesRetweetedToMe
       , statusesRetweetsOfMe
       , friendsIds
       , followerIds
       , listsAll
       , listsMembers
       , listsMembers'
       , userstream
       )
       where

import Web.Twitter.Enumerator.Types
import Web.Twitter.Enumerator.Monad
import Web.Twitter.Enumerator.Utils

import Web.Authenticate.OAuth

import Data.Aeson hiding (Error)
import qualified Data.Aeson.Types as AE

import Network.HTTP.Enumerator
import Data.Enumerator hiding (map, filter, drop, span)
import qualified Data.Enumerator.List as EL

import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Control.Monad.State
import Control.Applicative

api :: String -> [(ByteString, Maybe ByteString)] -> Iteratee ByteString IO a -> Manager -> TW (Iteratee ByteString IO a)
api url query iter mgr = do
  env <- get
  req <- liftIO $ signOAuth (twOAuth env) (twCredential env) =<< parseUrl url
  let req' = req { queryString = query, proxy = twProxy env }
  return $ http req' (\_ _ -> iter) mgr

statuses :: String -> [(ByteString, Maybe ByteString)] -> Manager -> TW (Iteratee ByteString IO [Status])
statuses url query = api aurl query iter
  where iter = enumLine =$ enumJSON =$ EL.map fromJSON' =$ skipNothing =$ EL.consume
        aurl = "https://api.twitter.com/1/statuses/" ++ url

statusesPublicTimeline = statuses "public_timeline.json"
statusesUserTimeline = statuses "user_timeline.json"
statusesFriendsTimeline = statuses "friends_timeline.json"
statusesReplies = statuses "replies.json"
statusesMentions = statuses "mentions.json"
statusesRetweetedByMe = statuses "retweeted_by_me.json"
statusesRetweetedToMe = statuses "retweeted_to_me.json"
statusesRetweetsOfMe = statuses "retweeted_of_me.json"

iterFold :: FromJSON a => Iteratee ByteString IO [a]
iterFold = enumLine =$ enumJSON =$ EL.map fromJSON' =$ skipNothing =$ EL.fold (++) []

friendsIds, followerIds :: Manager -> TW (Iteratee ByteString IO [UserId])
friendsIds = api "https://api.twitter.com/1/friends/ids.json" [] iterFold
followerIds = api "https://api.twitter.com/1/follower/ids.json" [] iterFold

listsAll :: Either String UserId -> Manager -> TW (Iteratee ByteString IO [List])
listsAll q =
  let query = either ((,) "screen_name" . Just . B8.pack) ((,) "user_id" . Just . B8.pack . show) q in
  api "https://api.twitter.com/1/lists/all.json" [query] iterFold

listsMembers :: Either String Integer -> Manager -> TW (Iteratee ByteString IO [User])
listsMembers q = withCursor iterFold $ listsMembers' q

listsMembers' :: Either String Integer -> Iteratee ByteString IO a -> Manager -> TW (Iteratee ByteString IO a)
listsMembers' q =
  api "http://api.twitter.com/1/lists/members.json" query
  where query = either mkSlug mkListId q
        mkSlug s =
          let (screenName, ln) = span (/= '/') s
              listName = drop 1 ln in
          [("slug", w listName), ("owner_screen_name", w screenName)]
        mkListId id = [("list_id", w . show $ id)]
        w = Just . B8.pack

data Cursor a =
  Cursor
  { cursorCurrent :: [a]
  , cursorPrev :: Maybe Integer
  , cursorNext :: Maybe Integer
  } deriving (Show, Eq)

withCursor :: FromJSON a
           => Iteratee ByteString IO a
           -> (Iteratee ByteString IO a -> Manager -> TW (Iteratee ByteString IO a))
           -> Manager -> TW (Iteratee ByteString IO a)
withCursor = undefined

parseCursor :: FromJSON a => Value -> AE.Parser (Cursor a)
parseCursor (Object o) =
  Cursor <$> o .: "user" <*> o .:? "prev" <*> o .:? "next"
parseCursor v@(Array arr) = return $ Cursor (maybe [] id $ fromJSON' v) Nothing Nothing

userstream :: Iteratee StreamingAPI IO a -> Manager -> TW (Iteratee ByteString IO a)
userstream iter = api "https://userstream.twitter.com/2/user.json" [] (enumLine =$ enumJSON =$ EL.map fromJSON' =$ skipNothing =$ iter)
