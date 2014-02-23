{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Stream
       (
       -- * StreamingAPI
         userstream
       , statusesFilterByFollow
       , statusesFilterByTrack
       -- , statusesFilterByLocation
       -- , statusesSample
       -- , statusesFirehose
       -- , sitestream
       -- , sitestream'
       , stream
  ) where

import Web.Twitter.Conduit.Base
import Web.Twitter.Conduit.Monad
import Web.Twitter.Types
import Web.Twitter.Conduit.Utils
import Web.Twitter.Conduit.Request

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Internal as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import Control.Monad.IO.Class
import Data.Aeson

($=+) :: MonadIO m
      => CI.ResumableSource m a
      -> CI.Conduit a m o
      -> m (CI.ResumableSource m o)
rsrc $=+ cndt = do
    (src, finalizer) <- C.unwrapResumable rsrc
    return $ CI.ResumableSource (src C.$= cndt) finalizer

stream :: (TwitterBaseM m, FromJSON responseType)
       => APIRequest apiName responseType
       -> TW m (C.ResumableSource (TW m) (APIResponse responseType))
stream (APIRequestGet u pa) = do
    rsrc <- api "GET" u pa
    rsrc $=+ (CL.sequence sinkJSON C.=$= CL.map APIResponse)
stream (APIRequestPost u pa) = do
    rsrc <- api "POST" u pa
    rsrc $=+ (CL.sequence sinkJSON C.=$= CL.map APIResponse)
stream (APIRequestPostMultipart _ _ _) =
    error "APIRequestPostMultipart is not supported by stream function."

data Userstream
userstream :: APIRequest Userstream StreamingAPI
userstream = APIRequestGet "https://userstream.twitter.com/1.1/user.json" []

statusesFilterEndpoint :: String
statusesFilterEndpoint = "https://stream.twitter.com/1.1/statuses/filter.json"

data StatusesFilter
statusesFilterByFollow :: [UserId] -> APIRequest StatusesFilter StreamingAPI
statusesFilterByFollow userIds =
    APIRequestPost statusesFilterEndpoint [("follow", S.intercalate "," . map showBS $ userIds)]

statusesFilterByTrack :: T.Text -- ^ keyword
                      -> APIRequest StatusesFilter StreamingAPI
statusesFilterByTrack keyword =
    APIRequestPost statusesFilterEndpoint [("track", T.encodeUtf8 keyword)]
