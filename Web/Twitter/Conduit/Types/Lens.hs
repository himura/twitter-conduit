{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Web.Twitter.Conduit.Types.Lens
       ( TT.Response
       , responseStatus
       , responseBody
       , responseHeaders
       , TT.WithCursor
       , previousCursor
       , nextCursor
       , contents
#if !MIN_VERSION_twitter_types(0,5,0)
       , TT.ImageSizeType
       , imageWidth
       , imageHeight
       , imageType
       , TT.UploadedMedia
       , mediaId
       , mediaSize
       , mediaImage
#endif
       , TT.TwitterErrorMessage
       , twitterErrorMessage
       , twitterErrorCode
       ) where

import qualified Web.Twitter.Conduit.Types as TT
import Web.Twitter.Conduit.Types.TH

makeLenses ''TT.WithCursor
#if !MIN_VERSION_twitter_types(0,5,0)
makeLenses ''TT.ImageSizeType
makeLenses ''TT.UploadedMedia
#endif
makeLenses ''TT.Response
makeLenses ''TT.TwitterErrorMessage
