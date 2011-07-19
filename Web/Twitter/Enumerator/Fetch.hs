{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Fetch
       ( api
       , QueryUser(..)
       , QueryList(..)
       , statusesPublicTimeline
       , statusesUserTimeline
       , statusesFriendsTimeline
       , statusesReplies
       , statusesMentions
       , statusesRetweetedByMe
       , statusesRetweetedToMe
       , statusesRetweetsOfMe
       , friendsIds
       , followersIds
       , listsAll
       , listsMembers
       , userstream
       )
       where

import Web.Twitter.Enumerator.Types
import Web.Twitter.Enumerator.Monad
import Web.Twitter.Enumerator.Utils

import Data.Aeson hiding (Error)
import qualified Data.Aeson.Types as AE

import Network.HTTP.Enumerator
import qualified Network.HTTP.Types as HT
import Data.Enumerator hiding (map, filter, drop, span, iterate)
import qualified Data.Enumerator.List as EL

import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Control.Monad.Trans
import Control.Applicative

import qualified Data.Map as M

data QueryUser = QUserId UserId | QScreenName String
               deriving (Show, Eq)
data QueryList = QListId Integer | QListName String
               deriving (Show, Eq)

api :: String -> HT.Query -> Iteratee ByteString IO a -> Iteratee ByteString TW a
api url query iter = do
  req <- lift $ apiRequest url query
  httpMgr req (\_ _ -> iter)

httpMgr :: Request IO
        -> (HT.Status
            -> HT.ResponseHeaders
            -> Iteratee ByteString IO a)
        -> Iteratee ByteString TW a
httpMgr req iterf = do
  mgr <- lift $ getManager
  liftTrans $ http req iterf mgr

apiRequest :: String -> HT.Query -> TW (Request IO)
apiRequest uri query = do
  p <- getProxy
  req <- liftIO $ parseUrl uri >>= \r -> return $ r { queryString = query, proxy = p }
  signOAuthTW req

statuses :: String -> HT.Query -> Enumerator Status TW a
statuses uri query = apiWithPages furi query 0
  where furi = "https://api.twitter.com/1/statuses/" ++ uri

apiWithPages :: (FromJSON a, Show a) => String -> HT.Query -> Integer -> Enumerator a TW b
apiWithPages uri query initPage =
  checkContinue1 go initPage
  where
    go loop page k = do
      let query' = insertQuery "page" (toMaybeByteString page) query
      req <- lift $ apiRequest uri query'
      liftIO . putStrLn . show . queryString $ req
      res <- lift $ run_ $ httpMgr req (\_ _ -> enumJSON =$ iterPageC)
      case res of
        Just [] -> k EOF
        Just xs -> k (Chunks xs) >>== loop (page + 1)
        Nothing -> k EOF

iterPageC :: (Monad m, FromJSON a) => Iteratee Value m (Maybe [a])
iterPageC = do
  ret <- EL.head
  case ret of
    Just v -> return . fromJSON' $ v
    Nothing -> return Nothing

insertQuery :: ByteString -> Maybe ByteString -> HT.Query -> HT.Query
insertQuery key value = mk
  where mk = M.toList . M.insert key value . M.fromList

statusesPublicTimeline = statuses "public_timeline.json"
statusesUserTimeline = statuses "user_timeline.json"
statusesFriendsTimeline = statuses "friends_timeline.json"
statusesReplies = statuses "replies.json"
statusesMentions = statuses "mentions.json"
statusesRetweetedByMe = statuses "retweeted_by_me.json"
statusesRetweetedToMe = statuses "retweeted_to_me.json"
statusesRetweetsOfMe = statuses "retweeted_of_me.json"

mkQueryUser :: QueryUser -> HT.Query
mkQueryUser (QUserId uid) =  [("user_id", toMaybeByteString uid)]
mkQueryUser (QScreenName sn) = [("screen_name", toMaybeByteString sn)]

mkQueryList :: QueryList -> HT.Query
mkQueryList (QListId lid) =  [("list_id", toMaybeByteString lid)]
mkQueryList (QListName listname) =
  [("slug", Just . B8.pack $ lstName),
   ("owner_screen_name", Just . B8.pack $ screenName)]
  where
    (screenName, ln) = span (/= '/') listname
    lstName = drop 1 ln

friendsIds, followersIds :: QueryUser -> Enumerator UserId TW a
friendsIds q = apiCursor "https://api.twitter.com/1/friends/ids.json" (mkQueryUser q) "ids" (-1)
followersIds q = apiCursor "https://api.twitter.com/1/followers/ids.json" (mkQueryUser q) "ids" (-1)

listsAll :: QueryUser -> Enumerator List TW a
listsAll q = apiCursor "https://api.twitter.com/1/lists/all.json" (mkQueryUser q) "" (-1)

listsMembers :: QueryList -> Enumerator User TW a
listsMembers q = apiCursor "http://api.twitter.com/1/lists/members.json" (mkQueryList q) "users" (-1)

data Cursor a =
  Cursor
  { cursorCurrent :: [a]
  , cursorPrev :: Maybe Integer
  , cursorNext :: Maybe Integer
  } deriving (Show, Eq)

iterCursor' :: (Monad m, FromJSON a) => T.Text -> Iteratee Value m (Maybe (Cursor a))
iterCursor' key = do
  ret <- EL.head
  case ret of
    Just v -> return . AE.parseMaybe (parseCursor key) $ v
    Nothing -> return Nothing

iterCursor :: (Monad m, FromJSON a) => T.Text -> Iteratee ByteString m (Maybe (Cursor a))
iterCursor key = enumLine =$ enumJSON =$ iterCursor' key

parseCursor :: FromJSON a => T.Text -> Value -> AE.Parser (Cursor a)
parseCursor key (Object o) =
  Cursor <$> o .: key <*> o .:? "previous_cursor" <*> o .:? "next_cursor"
parseCursor _ v@(Array _) = return $ Cursor (maybe [] id $ fromJSON' v) Nothing Nothing
parseCursor _ o = fail $ "Error at parseCursor: unknown object " ++ show o

apiCursor
  :: (FromJSON a, Show a) =>
     String
     -> HT.Query
     -> T.Text
     -> Integer
     -> Enumerator a TW b
apiCursor uri query cursorKey initCur =
  checkContinue1 go initCur
  where
    go loop cursor k = do
      let query' = insertQuery "cursor" (toMaybeByteString cursor) query
      req <- lift $ apiRequest uri query'
      liftIO . putStrLn . show . queryString $ req
      res <- lift $ run_ $ httpMgr req (\_ _ -> iterCursor cursorKey)
      case res of
        Just r -> do
          let nextCur = cursorNext r
              chunks = Chunks . cursorCurrent $ r
          case nextCur of
            -- TODO: clean up
            Just 0  -> k chunks
            Just nc -> k chunks >>== loop nc
            Nothing -> k chunks
        Nothing -> k EOF

userstream :: Iteratee StreamingAPI IO a -> Iteratee ByteString TW a
userstream iter = api "https://userstream.twitter.com/2/user.json" [] (enumLine =$ enumJSON =$ EL.map fromJSON' =$ skipNothing =$ iter)

toMaybeByteString :: Show a => a -> Maybe ByteString
toMaybeByteString = Just . B8.pack . show
