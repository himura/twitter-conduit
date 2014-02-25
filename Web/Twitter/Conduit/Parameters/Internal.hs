{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Web.Twitter.Conduit.Parameters.Internal
       ( Parameters(..)
       , readShow
       , wrappedParam
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

