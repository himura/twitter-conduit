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
import Web.Twitter.Conduit.Utils
import Web.Twitter.Conduit.Api

import qualified Network.HTTP.Types as HT
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Data.Monoid

searchSource :: TwitterBaseM m
             => String -- ^ search string
             -> HT.SimpleQuery -- ^ query
             -> TW m (SearchResult (C.Source (TW m) SearchStatus))
searchSource q commonQuery = searchSourceFrom q 1 commonQuery

searchSourceFrom :: TwitterBaseM m
                 => String -- ^ search string
                 -> Int -- ^ start page
                 -> HT.SimpleQuery -- ^ query
                 -> TW m (SearchResult (C.Source (TW m) SearchStatus))
searchSourceFrom q initPage commonQuery = do
    res <- search q initPage commonQuery
    let body = CL.sourceList (searchResultResults res) <>
               ((C.yield (searchResultNextPage res)) C.$= CL.concatMapM pull C.$= CL.concatMap id)
    return $ res { searchResultResults = body }
  where
    cqm = M.fromList commonQuery
    pull (Just query) = do
      let pq = HT.parseSimpleQuery query
          query' = M.toList $ M.union (M.fromList pq) cqm
      res <- search' query'
      remains <- pull $ searchResultNextPage res
      return $ searchResultResults res : remains
    pull Nothing = return []

search :: TwitterBaseM m
       => String -- ^ search string
       -> Int -- ^ page
       -> HT.SimpleQuery -- ^ query
       -> TW m (SearchResult [SearchStatus])
search q page query = search' query'
  where query' = ("q", B8.pack $ q) : ("page", showBS page) : query

search' :: TwitterBaseM m
        => HT.SimpleQuery -- ^ query
        -> TW m (SearchResult [SearchStatus])
search' query = apiGet signOAuthTW "search/tweets.json" query

directMessages :: TwitterBaseM m
               => HT.SimpleQuery -- ^ query
               -> C.Source (TW m) DirectMessage
directMessages query = apiWithPages signOAuthTW "direct_messages.json" query

friendsIds, followersIds
  :: TwitterBaseM m => UserParam -> C.Source (TW m) UserId
friendsIds   q = apiCursor signOAuthTW "friends/ids.json"   (mkUserParam q) "ids"
followersIds q = apiCursor signOAuthTW "followers/ids.json" (mkUserParam q) "ids"

usersShow :: TwitterBaseM m => UserParam -> TW m User
usersShow q = apiGet signOAuthTW "users/show.json" (mkUserParam q)

listsMembers :: TwitterBaseM m => ListParam -> C.Source (TW m) User
listsMembers q = apiCursor signOAuthTW "lists/members.json" (mkListParam q) "users"

