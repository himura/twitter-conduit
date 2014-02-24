{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Twitter.Conduit.Parameters
       ( Parameters(..)
       , wrappedParam
       , HasSinceIdParam(..)
       , HasCountParam(..)
       , HasMaxIdParam(..)
       , HasCursorParam(..)
       ) where

import Web.Twitter.Conduit.ParametersTH
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Control.Lens

-- | This 'Prism' convert from a 'ByteString' to some value based on 'Read' and 'Show'
--
-- >>> readShow # 2
-- "2"
-- >>> "1024" ^? readShow :: Maybe Integer
-- Just 1024
readShow :: (Read a, Show a) => Prism' S.ByteString a
readShow = prism' (S8.pack . show) (readMaybe . S8.unpack)
  where
    readMaybe str = case [x | (x, t) <- reads str, ("", "") <- lex t] of
        [x] -> Just x
        _ -> Nothing

defineHasParamClass "count" ''Integer 'readShow
defineHasParamClass "since_id" ''Integer 'readShow
defineHasParamClass "max_id" ''Integer 'readShow
defineHasParamClass "cursor" ''Integer 'readShow
