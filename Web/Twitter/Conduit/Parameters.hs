{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Web.Twitter.Conduit.Parameters
       ( Parameters(..)
       , wrappedParam
       , HasSinceIdParam(..)
       , HasCountParam(..)
       , HasMaxIdParam(..)
       , HasCursorParam(..)
       ) where

import qualified Network.HTTP.Types as HT
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Control.Lens

class Parameters a where
    params :: Lens' a HT.SimpleQuery

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

wrappedParam :: Parameters p => S.ByteString -> Prism' S.ByteString a -> Lens' p (Maybe a)
wrappedParam key aSBS = lens getter setter
   where
     getter = ((^? aSBS) =<<) . lookup key . view params
     setter = flip (over params . replace key)
     replace k (Just v) = ((k, aSBS # v):) . dropAssoc k
     replace k Nothing = dropAssoc k
     dropAssoc k = filter ((/= k) . fst)

class Parameters a => HasCountParam a where
    count :: Lens' a (Maybe Integer)
    count = wrappedParam "count" readShow
class Parameters a => HasSinceIdParam a where
    sinceId :: Lens' a (Maybe Integer)
    sinceId = wrappedParam "since_id" readShow
class Parameters a => HasMaxIdParam a where
    maxId :: Lens' a (Maybe Integer)
    maxId = wrappedParam "max_id" readShow
class Parameters a => HasCursorParam a where
    cursor :: Lens' a (Maybe Integer)
    cursor = wrappedParam "cursor" readShow
