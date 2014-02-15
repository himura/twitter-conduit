{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Fetch
       (
       -- * Search
         search
       , searchSource
       , searchSourceFrom

       -- * Direct Messages
       , directMessages
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
       , friendsList

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
       -- , listsStatuses
       -- , listsMemberships
       -- , listsSubscribers
       -- , listsSubscribersShow
       -- , listsMembersShow
       , listsMembers
       -- , lists
       -- , listsShow
       -- , listsSubscriptions
       ) where

import Web.Twitter.Types
import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Param
import Web.Twitter.Conduit.Api

import qualified Network.HTTP.Types as HT
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import Data.Monoid

searchSource :: TwitterBaseM m
             => String -- ^ search string
             -> HT.SimpleQuery -- ^ query
             -> TW m (SearchResult (C.Source (TW m) SearchStatus))
searchSource q = searchSourceFrom q ""

searchSourceFrom :: TwitterBaseM m
                 => String -- ^ search string
                 -> URIString -- ^ start page
                 -> HT.SimpleQuery -- ^ query
                 -> TW m (SearchResult (C.Source (TW m) SearchStatus))
searchSourceFrom q initPage commonQuery = do
    res <- search q initPage commonQuery
    let body = CL.sourceList (searchResultStatuses res) <>
               (C.yield (searchMetadataNextResults . searchResultSearchMetadata $ res) C.$= CL.concatMapM pull C.$= CL.concatMap id)
    return $ res { searchResultStatuses = body }
  where
    cqm = M.fromList commonQuery
    pull (Just query) = do
      let pq = HT.parseSimpleQuery . T.encodeUtf8 $ query
          query' = M.toList $ M.union (M.fromList pq) cqm
      res <- search' query'
      remains <- pull $ searchMetadataNextResults . searchResultSearchMetadata $ res
      return $ searchResultStatuses res : remains
    pull Nothing = return []

search :: TwitterBaseM m
       => String -- ^ search string
       -> URIString -- ^ next query
       -> HT.SimpleQuery -- ^ query
       -> TW m (SearchResult [SearchStatus])
search q next_results_str query = search' query'
  where
    next_query = HT.parseSimpleQuery . T.encodeUtf8 $ next_results_str
    query' = ("q", B8.pack q) : (next_query ++ query)

search' :: TwitterBaseM m
        => HT.SimpleQuery -- ^ query
        -> TW m (SearchResult [SearchStatus])
search' = apiGet "search/tweets.json"

directMessages :: TwitterBaseM m
               => HT.SimpleQuery -- ^ query
               -> C.Source (TW m) DirectMessage
directMessages = apiWithPages "direct_messages.json"

friendsIds, followersIds
  :: TwitterBaseM m => UserParam -> C.Source (TW m) UserId
friendsIds   q = apiCursor "friends/ids.json"   (mkUserParam q) "ids"
followersIds q = apiCursor "followers/ids.json" (mkUserParam q) "ids"

friendsList
  :: TwitterBaseM m => UserParam -> C.Source (TW m) User
friendsList q = apiCursor "friends/list.json" (mkUserParam q) "users"

usersShow :: TwitterBaseM m => UserParam -> TW m User
usersShow q = apiGet "users/show.json" (mkUserParam q)

listsMembers :: TwitterBaseM m => ListParam -> C.Source (TW m) User
listsMembers q = apiCursor "lists/members.json" (mkListParam q) "users"

