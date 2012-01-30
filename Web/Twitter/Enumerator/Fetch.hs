{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Fetch
       ( QueryUser(..)
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

import Control.Monad.Trans
import Control.Applicative
import Data.Aeson hiding (Error)
import qualified Data.Aeson.Types as AE
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

data QueryUser = QUserId UserId | QScreenName String
               deriving (Show, Eq)
data QueryList = QListId Integer | QListName String
               deriving (Show, Eq)


apiGet :: FromJSON a => String -> HT.Query -> C.Sink a IO b -> TW b
apiGet uri query iter = undefined -- run_ $ api "GET" uri query (handleParseError iter')
  -- where iter' = enumJSON =$ EL.map fromJSON' =$ skipNothing =$ iter

statuses :: (FromJSON a, Show a) => String -> HT.Query -> C.Source TW a
statuses uri query = apiWithPages furi query 1
  where furi = endpoint ++ "statuses/" ++ uri

apiWithPages :: (FromJSON a, Show a) => String -> HT.Query -> Integer -> C.Source TW a
apiWithPages uri query initPage = undefined
{-
apiWithPages uri query initPage =
  checkContinue1 go initPage
  where
    go loop page k = do
      let query' = insertQuery "page" (toMaybeByteString page) query
      res <- lift $ run_ $ api "GET" uri query' (handleParseError (enumJSON =$ iterPageC))
      case res of
        Just [] -> k EOF
        Just xs -> k (Chunks xs) >>== loop (page + 1)
        Nothing -> k EOF
-}

iterPageC :: (C.Resource m, Monad m, FromJSON a) => C.Sink Value m (Maybe [a])
iterPageC = do
  ret <- CL.head
  case ret of
    Just v -> return . fromJSON' $ v
    Nothing -> return Nothing

insertQuery :: ByteString -> Maybe ByteString -> HT.Query -> HT.Query
insertQuery key value = mk
  where mk = M.toList . M.insert key value . M.fromList

statusesHomeTimeline :: HT.Query -> C.Source TW Status
statusesHomeTimeline = statuses "home_timeline.json"

statusesMentions :: HT.Query -> C.Source TW Status
statusesMentions = statuses "mentions.json"

statusesPublicTimeline :: HT.Query -> C.Source TW Status
statusesPublicTimeline = statuses "public_timeline.json"

statusesRetweetedByMe :: HT.Query -> C.Source TW Status
statusesRetweetedByMe = statuses "retweeted_by_me.json"

statusesRetweetedToMe :: HT.Query -> C.Source TW Status
statusesRetweetedToMe = statuses "retweeted_to_me.json"

statusesRetweetsOfMe :: HT.Query -> C.Source TW Status
statusesRetweetsOfMe = statuses "retweeted_of_me.json"

statusesUserTimeline :: HT.Query -> C.Source TW Status
statusesUserTimeline = statuses "user_timeline.json"

statusesRetweetedToUser :: HT.Query -> C.Source TW Status
statusesRetweetedToUser = statuses "retweeted_to_user.json"

statusesRetweetedByUser :: HT.Query -> C.Source TW Status
statusesRetweetedByUser = statuses "retweeted_by_user.json"

statusesIdRetweetedBy :: StatusId -> HT.Query -> C.Source TW User
statusesIdRetweetedBy status_id = statuses (show status_id ++ "/retweeted_by.json")

statusesIdRetweetedByIds :: StatusId -> HT.Query -> C.Source TW UserId
statusesIdRetweetedByIds status_id = statuses (show status_id ++ "/retweeted_by/ids.json")

statusesRetweetsId :: StatusId -> HT.Query -> TW [RetweetedStatus]
statusesRetweetsId status_id query = undefined -- apiGet uri query CL.head_
  -- where uri = endpoint ++ "statuses/retweets/" ++ show status_id ++ ".json"

statusesShowId :: StatusId -> HT.Query -> TW Status
statusesShowId status_id query = undefined -- apiGet (endpoint ++ "statuses/show/" ++ show status_id ++ ".json") query CL.head_

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

friendsIds, followersIds :: QueryUser -> C.Source TW UserId
friendsIds q = apiCursor (endpoint ++ "friends/ids.json") (mkQueryUser q) "ids" (-1)
followersIds q = apiCursor (endpoint ++ "followers/ids.json") (mkQueryUser q) "ids" (-1)

usersShow :: QueryUser -> TW User
usersShow q = undefined -- apiGet (endpoint ++ "users/show.json") (mkQueryUser q) CL.head_

listsAll :: QueryUser -> C.Source TW List
listsAll q = apiCursor (endpoint ++ "lists/all.json") (mkQueryUser q) "" (-1)

listsMembers :: QueryList -> C.Source TW User
listsMembers q = apiCursor (endpoint ++ "lists/members.json") (mkQueryList q) "users" (-1)

data Cursor a =
  Cursor
  { cursorCurrent :: [a]
  , cursorPrev :: Maybe Integer
  , cursorNext :: Maybe Integer
  } deriving (Show, Eq)

iterCursor' :: (C.Resource m, Monad m, FromJSON a) => T.Text -> C.Sink Value m (Maybe (Cursor a))
iterCursor' key = do
  ret <- CL.head
  case ret of
    Just v -> return . AE.parseMaybe (parseCursor key) $ v
    Nothing -> return Nothing

iterCursor :: (C.Resource m, Monad m, FromJSON a) => T.Text -> C.Sink ByteString m (Maybe (Cursor a))
iterCursor key = undefined -- enumLine C.=$ handleParseError (enumJSON C.=$ iterCursor' key)

handleParseError :: Monad m => C.Sink ByteString m b -> C.Sink ByteString m b
handleParseError iter = undefined {- iter `catchError` hndl
  where
    getChunk = continue return
    hndl e = getChunk >>= \x -> case x of
      Chunks xs -> throwError $ ParserException e xs
      _ -> throwError $ ParserException e []
-}

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
     -> C.Source TW a
apiCursor uri query cursorKey initCur = undefined {-
  checkContinue1 go initCur
  where
    go loop cursor k = do
      let query' = insertQuery "cursor" (toMaybeByteString cursor) query
      res <- lift $ run_ $ api "GET" uri query' (iterCursor cursorKey)
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
-}

streamingConduit :: C.ResourceThrow m => C.Conduit ByteString m StreamingAPI
streamingConduit = conduitParser json C.=$= CL.concatMap (maybeToList . fromJSON')

userstream :: C.ResourceT TW (C.Source IO StreamingAPI)
userstream = do
  src <- api "GET" "https://userstream.twitter.com/2/user.json" ""
  return $ src C.$= streamingConduit

statusesFilter :: HT.Ascii -> C.ResourceT TW (C.Source IO StreamingAPI)
statusesFilter query = do
  src <- api "GET" "https://stream.twitter.com/1/statuses/filter.json" query
  return $ src C.$= streamingConduit

toMaybeByteString :: Show a => a -> Maybe ByteString
toMaybeByteString = Just . B8.pack . show
