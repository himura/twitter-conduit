{-# LANGUAGE RankNTypes #-}

module Web.Twitter.Conduit.Lens (
    -- * 'TT.Response'
    TT.APIResponse,
    responseStatus,
    responseBody,
    responseHeaders,

    -- * 'TT.TwitterErrorMessage'
    TT.TwitterError,
    twitterErrorMessage,
    twitterErrorCode,

    -- * 'TT.WithCursor'
    TT.WithCursor,
    previousCursor,
    nextCursor,
    contents,
) where

import Control.Lens (Lens, Lens')
import Data.Text (Text)
import Network.HTTP.Types (ResponseHeaders, Status)
import qualified Web.Twitter.Conduit.Cursor as TT
import qualified Web.Twitter.Conduit.Response as TT

-- * Lenses for 'TT.Response'
responseStatus :: forall responseType. Lens' (TT.APIResponse responseType) Status
responseStatus afb s = (\b -> s {TT.apiResponseStatus = b}) <$> afb (TT.apiResponseStatus s)

responseHeaders :: forall responseType. Lens' (TT.APIResponse responseType) ResponseHeaders
responseHeaders afb s = (\b -> s {TT.apiResponseHeaders = b}) <$> afb (TT.apiResponseHeaders s)

responseBody :: forall a b. Lens (TT.APIResponse a) (TT.APIResponse b) a b
responseBody afb s = (\b -> s {TT.apiResponseBody = b}) <$> afb (TT.apiResponseBody s)
-- * Lenses for 'TT.TwitterErrorMessage'

twitterErrorCode :: Lens' TT.TwitterError Int
twitterErrorCode afb s = (\b -> s {TT.twitterErrorCode = b}) <$> afb (TT.twitterErrorCode s)

twitterErrorMessage :: Lens' TT.TwitterError Text
twitterErrorMessage afb s = (\b -> s {TT.twitterErrorMessage = b}) <$> afb (TT.twitterErrorMessage s)
-- * Lenses for 'TT.WithCursor'
previousCursor :: forall cursorType cursorKey wrapped. Lens' (TT.WithCursor cursorType cursorKey wrapped) (Maybe cursorType)
previousCursor afb s = (\b -> s {TT.previousCursor = b}) <$> afb (TT.previousCursor s)

nextCursor :: forall cursorType cursorKey wrapped. Lens' (TT.WithCursor cursorType cursorKey wrapped) (Maybe cursorType)
nextCursor afb s = (\b -> s {TT.nextCursor = b}) <$> afb (TT.nextCursor s)

contents :: forall cursorType cursorKey a b. Lens (TT.WithCursor cursorType cursorKey a) (TT.WithCursor cursorType cursorKey b) [a] [b]
contents afb s = (\b -> s {TT.contents = b}) <$> afb (TT.contents s)
