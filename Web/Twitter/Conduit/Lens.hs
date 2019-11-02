{-# LANGUAGE RankNTypes #-}

module Web.Twitter.Conduit.Lens
       (
       -- * 'TT.Response'
         TT.Response
       , responseStatus
       , responseBody
       , responseHeaders
       -- * 'TT.TwitterErrorMessage'
       , TT.TwitterErrorMessage
       , twitterErrorMessage
       , twitterErrorCode

       -- * 'TT.WithCursor'
       , TT.WithCursor
       , previousCursor
       , nextCursor
       , contents

       -- * Re-exports
       , TT.TwitterError(..)
       , TT.CursorKey (..)
       , TT.IdsCursorKey
       , TT.UsersCursorKey
       , TT.ListsCursorKey
       ) where

import Control.Lens
import Data.Text (Text)
import Network.HTTP.Types (Status, ResponseHeaders)
import qualified Web.Twitter.Conduit.Cursor as TT
import qualified Web.Twitter.Conduit.Response as TT

-- * Lenses for 'TT.Response'
responseStatus :: forall responseType. Lens' (TT.Response responseType) Status
responseStatus afb s = (\b -> s { TT.responseStatus = b }) <$> afb (TT.responseStatus s)

responseHeaders :: forall responseType. Lens' (TT.Response responseType) ResponseHeaders
responseHeaders afb s = (\b -> s {TT.responseHeaders = b }) <$> afb (TT.responseHeaders s)

responseBody :: forall a b. Lens (TT.Response a) (TT.Response b) a b
responseBody afb s = (\b -> s { TT.responseBody = b }) <$> afb (TT.responseBody s)

-- * Lenses for 'TT.TwitterErrorMessage'

twitterErrorCode :: Lens' TT.TwitterErrorMessage Int
twitterErrorCode afb s = (\b -> s { TT.twitterErrorCode = b }) <$> afb (TT.twitterErrorCode s)

twitterErrorMessage :: Lens' TT.TwitterErrorMessage Text
twitterErrorMessage afb s = (\b -> s { TT.twitterErrorMessage = b }) <$> afb (TT.twitterErrorMessage s)

-- * Lenses for 'TT.WithCursor'
previousCursor :: forall cursorType cursorKey wrapped. Lens' (TT.WithCursor cursorType cursorKey wrapped) (Maybe cursorType)
previousCursor afb s = (\b -> s { TT.previousCursor = b }) <$> afb (TT.previousCursor s)

nextCursor :: forall cursorType cursorKey wrapped. Lens' (TT.WithCursor cursorType cursorKey wrapped) (Maybe cursorType)
nextCursor afb s = (\b -> s { TT.nextCursor = b }) <$> afb (TT.nextCursor s)

contents :: forall cursorType cursorKey a b. Lens (TT.WithCursor cursorType cursorKey a) (TT.WithCursor cursorType cursorKey b) [a] [b]
contents afb s = (\b -> s { TT.contents = b }) <$> afb (TT.contents s)
