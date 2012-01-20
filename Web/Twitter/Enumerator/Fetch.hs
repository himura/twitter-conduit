{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Fetch
       ( api
       , QueryUser(..)
       , QueryList(..)

       -- * Timelines
       , statusesHomeTimeline
       , statusesMentions
       , statusesPublicTimeline
       , statusesRetweetedByMe
       , statusesRetweetedToMe
       , statusesRetweetsOfMe
       , statusesUserTimeline
       , statusesRetweetedToUser
       , statusesRetweetedByUser

       -- * Tweets
       , statusesIdRetweetedBy
       , statusesIdRetweetedByIds
       , statusesRetweetsId
       , statusesShowId

       -- * Search
       -- , search

       -- * Direct Messages
       -- , directMessages
       -- , directMessagesSent
       -- , directMessagesShowId

       -- * Friends & Followers
       , friendsIds
       , followersIds
       -- , friendshipsExists
       -- , friendshipsIncoming
       -- , friendshipsOutgoing
       -- , friendshipsShow
       -- , friendshipsLookup
       -- , friendshipsNoRetweetIds

       -- * Users
       -- , usersLookup
       -- , usersProfileImageScreenName
       -- , usersSearch
       , usersShow
       -- , usersContributees
       -- , usersContributors

       -- * Suggested Users
       -- , usersSuggestions
       -- , usersSuggestionsSlug
       -- , usersSuggestionsSlugMembers

       -- * Favorites
       -- , favorites

       -- * Lists
       , listsAll
       -- , listsStatuses
       -- , listsMemberships
       -- , listsSubscribers
       -- , listsSubscribersShow
       -- , listsMembersShow
       , listsMembers
       -- , lists
       -- , listsShow
       -- , listsSubscriptions

       -- * StreamingAPI
       , userstream
       , statusesFilter
       )
       where

import Web.Twitter.Enumerator.Types
import Web.Twitter.Enumerator.Monad
import Web.Twitter.Enumerator.Utils
import Web.Twitter.Enumerator.Api

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


apiGet :: FromJSON a => String -> HT.Query -> Iteratee a IO b -> TW b
apiGet uri query iter = run_ $ api (B8.pack "GET") uri query (handleParseError iter')
  where iter' = enumJSON =$ EL.map fromJSON' =$ skipNothing =$ iter

statuses :: (FromJSON a, Show a) => String -> HT.Query -> Enumerator a TW b
statuses uri query = apiWithPages furi query 1
  where furi = endpoint ++ "statuses/" ++ uri

apiWithPages :: (FromJSON a, Show a) => String -> HT.Query -> Integer -> Enumerator a TW b
apiWithPages uri query initPage =
  checkContinue1 go initPage
  where
    go loop page k = do
      let query' = insertQuery "page" (toMaybeByteString page) query
      res <- lift $ run_ $ api (B8.pack "GET") uri query' (handleParseError (enumJSON =$ iterPageC))
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

statusesHomeTimeline :: HT.Query -> Enumerator Status TW a
statusesHomeTimeline = statuses "home_timeline.json"

statusesMentions :: HT.Query -> Enumerator Status TW a
statusesMentions = statuses "mentions.json"

statusesPublicTimeline :: HT.Query -> Enumerator Status TW a
statusesPublicTimeline = statuses "public_timeline.json"

statusesRetweetedByMe :: HT.Query -> Enumerator Status TW a
statusesRetweetedByMe = statuses "retweeted_by_me.json"

statusesRetweetedToMe :: HT.Query -> Enumerator Status TW a
statusesRetweetedToMe = statuses "retweeted_to_me.json"

statusesRetweetsOfMe :: HT.Query -> Enumerator Status TW a
statusesRetweetsOfMe = statuses "retweeted_of_me.json"

statusesUserTimeline :: HT.Query -> Enumerator Status TW a
statusesUserTimeline = statuses "user_timeline.json"

statusesRetweetedToUser :: HT.Query -> Enumerator Status TW a
statusesRetweetedToUser = statuses "retweeted_to_user.json"

statusesRetweetedByUser :: HT.Query -> Enumerator Status TW a
statusesRetweetedByUser = statuses "retweeted_by_user.json"

statusesIdRetweetedBy :: StatusId -> HT.Query -> Enumerator User TW a
statusesIdRetweetedBy status_id = statuses (show status_id ++ "/retweeted_by.json")

statusesIdRetweetedByIds :: StatusId -> HT.Query -> Enumerator UserId TW a
statusesIdRetweetedByIds status_id = statuses (show status_id ++ "/retweeted_by/ids.json")

statusesRetweetsId :: StatusId -> HT.Query -> TW [RetweetedStatus]
statusesRetweetsId status_id query = apiGet uri query EL.head_
  where uri = endpoint ++ "statuses/retweets/" ++ show status_id ++ ".json"

statusesShowId :: StatusId -> HT.Query -> TW Status
statusesShowId status_id query = apiGet (endpoint ++ "statuses/show/" ++ show status_id ++ ".json") query EL.head_

mkQueryUser :: QueryUser -> HT.Query
mkQueryUser (QUserId uid) =  [("user_id", toMaybeByteString uid)]
mkQueryUser (QScreenName sn) = [("screen_name", Just . B8.pack $ sn)]

mkQueryList :: QueryList -> HT.Query
mkQueryList (QListId lid) =  [("list_id", toMaybeByteString lid)]
mkQueryList (QListName listname) =
  [("slug", Just . B8.pack $ lstName),
   ("owner_screen_name", Just . B8.pack $ screenName)]
  where
    (screenName, ln) = span (/= '/') listname
    lstName = drop 1 ln

friendsIds, followersIds :: QueryUser -> Enumerator UserId TW a
friendsIds q = apiCursor (endpoint ++ "friends/ids.json") (mkQueryUser q) "ids" (-1)
followersIds q = apiCursor (endpoint ++ "followers/ids.json") (mkQueryUser q) "ids" (-1)

usersShow :: QueryUser -> TW User
usersShow q = apiGet (endpoint ++ "users/show.json") (mkQueryUser q) EL.head_

listsAll :: QueryUser -> Enumerator List TW a
listsAll q = apiCursor (endpoint ++ "lists/all.json") (mkQueryUser q) "" (-1)

listsMembers :: QueryList -> Enumerator User TW a
listsMembers q = apiCursor (endpoint ++ "lists/members.json") (mkQueryList q) "users" (-1)

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
iterCursor key = enumLine =$ handleParseError (enumJSON =$ iterCursor' key)

handleParseError :: Monad m => Iteratee ByteString m b -> Iteratee ByteString m b
handleParseError iter = iter `catchError` hndl
  where
    getChunk = continue return
    hndl e = getChunk >>= \x -> case x of
      Chunks xs -> throwError $ PerserException e xs
      _ -> throwError $ PerserException e []

parseCursor :: FromJSON a => T.Text -> Value -> AE.Parser (Cursor a)
parseCursor key (Object o) =
  checkError o
  <|>
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
      res <- lift $ run_ $ api (B8.pack "GET") uri query' (iterCursor cursorKey)
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

{-# SPECIALIZE apiIter ::  Iteratee StreamingAPI IO b -> Iteratee ByteString IO b #-}
apiIter :: (FromJSON a, Monad m) => Iteratee a m b -> Iteratee ByteString m b
apiIter iter = enumLine =$ handleParseError (enumJSON =$ EL.map fromJSON' =$ skipNothing =$ iter)

userstream :: Iteratee StreamingAPI IO a -> Iteratee ByteString TW a
userstream = api (B8.pack "GET") "https://userstream.twitter.com/2/user.json" [] . apiIter

statusesFilter :: HT.Query -> Iteratee StreamingAPI IO a -> Iteratee ByteString TW a
statusesFilter query = api (B8.pack "GET") "https://stream.twitter.com/1/statuses/filter.json" query . apiIter

toMaybeByteString :: Show a => a -> Maybe ByteString
toMaybeByteString = Just . B8.pack . show
