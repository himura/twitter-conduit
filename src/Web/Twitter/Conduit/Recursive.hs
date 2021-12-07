{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Twitter.Conduit.Recursive where

import Control.Lens (Field2 (_2), minimumOf, traversed, (%~), (&), (.~), (?~), (^.))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, Value)
import Data.Aeson.Lens (AsNumber (_Integer), key)
import Data.Coerce (coerce)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import GHC.TypeLits (KnownSymbol)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Types as HTTPTypes
import Web.Twitter.Conduit.Cursor (WithCursor (contents, nextCursor))
import Web.Twitter.Conduit.Internal (call, call')
import Web.Twitter.Conduit.Internal.APIRequest (APIRequest (..), BodyEmpty, HasParam, PV (..))
import Web.Twitter.Conduit.Types (TWInfo)
import Web.Twitter.Types.Lens (
    AsStatus (status_id),
    SearchResult,
    searchMetadataNextResults,
    searchResultSearchMetadata,
    searchResultStatuses,
 )

-- | A wrapper function to perform multiple API request with changing @max_id@ parameter.
--
-- This function cooperate with instances of 'HasMaxIdParam'.
sourceWithMaxId ::
    ( MonadIO m
    , FromJSON responseType
    , AsStatus responseType
    , HasParam "max_id" Integer supports
    ) =>
    -- | Twitter Setting
    TWInfo ->
    Manager ->
    APIRequest supports BodyEmpty [responseType] ->
    C.ConduitT () responseType m ()
sourceWithMaxId info mgr = loop
  where
    loop req = do
        res <- liftIO $ call info mgr req
        case getMinId res of
            Just mid -> do
                CL.sourceList res
                loop $ req & #max_id ?~ mid - 1
            Nothing -> CL.sourceList res
    getMinId = minimumOf (traverse . status_id)

-- | A wrapper function to perform multiple API request with changing @max_id@ parameter.
-- The response of this function is not restrict to @responseType@,
-- so you can choose an arbitrarily type of FromJSON instances.
--
-- This function cooperate with instances of 'HasMaxIdParam'.
sourceWithMaxId' ::
    ( MonadIO m
    , HasParam "max_id" Integer supports
    ) =>
    -- | Twitter Setting
    TWInfo ->
    Manager ->
    APIRequest supports BodyEmpty [responseType] ->
    C.ConduitT () Value m ()
sourceWithMaxId' info mgr = loop
  where
    loop req = do
        (res :: [Value]) <- liftIO $ call' info mgr req
        case minimumOf (traverse . key "id" . _Integer) res of
            Just mid -> do
                CL.sourceList res
                loop $ req & #max_id ?~ mid - 1
            Nothing -> CL.sourceList res

-- | A wrapper function to perform multiple API request with changing @cursor@ parameter.
--
-- This function cooperate with instances of 'HasCursorParam'.
sourceWithCursor ::
    ( MonadIO m
    , FromJSON responseType
    , KnownSymbol ck
    , HasParam "cursor" Integer supports
    ) =>
    -- | Twitter Setting
    TWInfo ->
    Manager ->
    APIRequest supports BodyEmpty (WithCursor Integer ck responseType) ->
    C.ConduitT () responseType m ()
sourceWithCursor info mgr req = loop (Just (-1))
  where
    loop Nothing = CL.sourceNull
    loop (Just 0) = CL.sourceNull
    loop (Just cur) = do
        res <- liftIO $ call info mgr $ req & #cursor ?~ cur
        CL.sourceList $ contents res
        loop $ nextCursor res

-- | A wrapper function to perform multiple API request with changing @cursor@ parameter.
-- The response of this function is not restrict to @responseType@,
-- so you can choose an arbitrarily type of FromJSON instances.
--
-- This function cooperate with instances of 'HasCursorParam'.
sourceWithCursor' ::
    ( MonadIO m
    , KnownSymbol ck
    , HasParam "cursor" Integer supports
    ) =>
    -- | Twitter Setting
    TWInfo ->
    Manager ->
    APIRequest supports BodyEmpty (WithCursor Integer ck responseType) ->
    C.ConduitT () Value m ()
sourceWithCursor' info mgr req = loop (Just (-1))
  where
    relax ::
        APIRequest apiName BodyEmpty (WithCursor Integer ck responseType) ->
        APIRequest apiName BodyEmpty (WithCursor Integer ck Value)
    relax = coerce
    loop Nothing = CL.sourceNull
    loop (Just 0) = CL.sourceNull
    loop (Just cur) = do
        res <- liftIO $ call info mgr $ relax $ req & #cursor ?~ cur
        CL.sourceList $ contents res
        loop $ nextCursor res

-- | A wrapper function to perform multiple API request with @SearchResult@.
sourceWithSearchResult ::
    ( MonadIO m
    , FromJSON responseType
    ) =>
    -- | Twitter Setting
    TWInfo ->
    Manager ->
    APIRequest supports BodyEmpty (SearchResult [responseType]) ->
    m (SearchResult (C.ConduitT () responseType m ()))
sourceWithSearchResult info mgr req = do
    res <- liftIO $ call info mgr req
    let body =
            CL.sourceList (res ^. searchResultStatuses)
                <> loop (res ^. searchResultSearchMetadata . searchMetadataNextResults)
    return $ res & searchResultStatuses .~ body
  where
    origQueryMap = M.fromList $ apiRequestParams req
    loop Nothing = CL.sourceNull
    loop (Just nextResultsStr) = do
        let nextResults = nextResultsStr & HTTPTypes.parseSimpleQuery . T.encodeUtf8 & traversed . _2 %~ (PVString . T.decodeUtf8)
            nextParams = M.toList $ M.union (M.fromList nextResults) origQueryMap
        res <- liftIO $ call info mgr $ req {apiRequestParams = nextParams}
        CL.sourceList (res ^. searchResultStatuses)
        loop $ res ^. searchResultSearchMetadata . searchMetadataNextResults

-- | A wrapper function to perform multiple API request with @SearchResult@.
sourceWithSearchResult' ::
    ( MonadIO m
    ) =>
    -- | Twitter Setting
    TWInfo ->
    Manager ->
    APIRequest supports BodyEmpty (SearchResult [responseType]) ->
    m (SearchResult (C.ConduitT () Value m ()))
sourceWithSearchResult' info mgr req = do
    res <- liftIO $ call info mgr $ relax req
    let body =
            CL.sourceList (res ^. searchResultStatuses)
                <> loop (res ^. searchResultSearchMetadata . searchMetadataNextResults)
    return $ res & searchResultStatuses .~ body
  where
    origQueryMap = M.fromList $ apiRequestParams req
    relax ::
        APIRequest apiName BodyEmpty (SearchResult [responseType]) ->
        APIRequest apiName BodyEmpty (SearchResult [Value])
    relax = coerce
    loop Nothing = CL.sourceNull
    loop (Just nextResultsStr) = do
        let nextResults = nextResultsStr & HTTPTypes.parseSimpleQuery . T.encodeUtf8 & traversed . _2 %~ (PVString . T.decodeUtf8)
            nextParams = M.toList $ M.union (M.fromList nextResults) origQueryMap
        res <- liftIO $ call info mgr $ relax $ req {apiRequestParams = nextParams}
        CL.sourceList (res ^. searchResultStatuses)
        loop $ res ^. searchResultSearchMetadata . searchMetadataNextResults
