{-# LANGUAGE TemplateHaskell #-}

module Web.Twitter.Conduit.Types.Lens
       ( TT.Response
       , responseStatus
       , responseBody
       , responseHeaders
       , TT.WithCursor
       , previousCursor
       , nextCursor
       , contents
       , TT.ImageSizeType
       , imageWidth
       , imageHeight
       , imageType
       , TT.UploadedMedia
       , mediaId
       , mediaSize
       , mediaImage
       , TT.TwitterErrorMessage
       , twitterErrorMessage
       , twitterErrorCode
       ) where

import qualified Web.Twitter.Conduit.Types as TT
import Web.Twitter.Conduit.Types.TH

makeLenses ''TT.WithCursor
makeLenses ''TT.ImageSizeType
makeLenses ''TT.UploadedMedia
makeLenses ''TT.Response
makeLenses ''TT.TwitterErrorMessage
