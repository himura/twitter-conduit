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
import Network.HTTP.Types (Query)
import Data.Enumerator hiding (map, filter, drop, span, iterate)
import qualified Data.Enumerator.List as EL

import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Control.Monad.State
import Control.Applicative

import qualified Data.Map as M

api :: String -> Query -> Iteratee ByteString IO a -> Manager -> TW (Iteratee ByteString IO a)
api url query iter mgr = do
  req <- get >>= liftIO . apiRequest url query
  return $ http req (\_ _ -> iter) mgr

apiRequest :: String -> Query -> TWEnv -> IO (Request IO)
apiRequest uri query env = do
  req <- parseUrl uri >>= \r -> return $ r { queryString = query, proxy = twProxy env }
  signOAuth (twOAuth env) (twCredential env) $ req

statuses :: String -> Query -> Manager -> TW (Iteratee ByteString IO [Status])
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
iterFold = enumLine =$ enumJSON =$ EL.map fromJSON' =$ skipNothing =$ EL.consume

friendsIds, followerIds :: Manager -> TW (Iteratee ByteString IO [UserId])
friendsIds = api "https://api.twitter.com/1/friends/ids.json" [] iterFold
followerIds = api "https://api.twitter.com/1/follower/ids.json" [] iterFold

listsAll :: Either String UserId -> Manager -> TW (Iteratee ByteString IO [List])
listsAll q =
  let query = either ((,) "screen_name" . Just . B8.pack) ((,) "user_id" . Just . B8.pack . show) q in
  api "https://api.twitter.com/1/lists/all.json" [query] iterFold

listsMembers :: Either String Integer -> Manager -> TW (Enumerator User IO a)
listsMembers q mgr =
  apiCursor "http://api.twitter.com/1/lists/members.json" query "users" (-1) mgr
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

iterCursor' :: FromJSON a => T.Text -> Iteratee Value IO (Maybe (Cursor a))
iterCursor' key = do
  ret <- EL.head
  case ret of
    Just v -> return . AE.parseMaybe (parseCursor key) $ v
    Nothing -> return Nothing

iterCursor :: FromJSON a => T.Text -> Iteratee ByteString IO (Maybe (Cursor a))
iterCursor key = enumLine =$ enumJSON =$ iterCursor' key

addCursor :: Integer -> Query -> Query
addCursor cursor = nq
  where nq = M.toList . M.insert "cursor" (Just strcur) . M.fromList
        strcur = B8.pack . show $ cursor
parseCursor :: FromJSON a => T.Text -> Value -> AE.Parser (Cursor a)
parseCursor key (Object o) =
  Cursor <$> o .: key <*> o .:? "previous_cursor" <*> o .:? "next_cursor"
parseCursor _ v@(Array arr) = return $ Cursor (maybe [] id $ fromJSON' v) Nothing Nothing

apiCursor
  :: (FromJSON a, Show a) =>
     String
     -> Query
     -> T.Text
     -> Integer
     -> Manager
     -> TW (Enumerator a IO b)
apiCursor uri query cursorKey initCur mgr = do
  env <- get
  return $ checkContinue1 go (env, initCur)
  where
    go loop s k = do
      let (env, cursor) = s
          query' = addCursor cursor query
      req <- liftIO $ apiRequest uri query' env
      liftIO . putStrLn . show . queryString $ req
      res <- liftIO $ withManager (\mgr -> run_ $ http req (\_ _ -> iterCursor cursorKey) mgr)
      case res of
        Just r -> do
          let nextCur = cursorNext r
              chunks = Chunks . cursorCurrent $ r
          case nextCur of
            -- TODO: clean up
            Just 0  -> k chunks
            Just nc -> k chunks >>== loop (env, nc)
            Nothing -> k chunks
        Nothing -> k EOF

userstream :: Iteratee StreamingAPI IO a -> Manager -> TW (Iteratee ByteString IO a)
userstream iter = api "https://userstream.twitter.com/2/user.json" [] (enumLine =$ enumJSON =$ EL.map fromJSON' =$ skipNothing =$ iter)
