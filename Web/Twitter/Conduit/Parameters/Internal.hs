{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Web.Twitter.Conduit.Parameters.Internal
       ( Parameters(..)
       , readShow
       , booleanQuery
       , integerArrayQuery
       , wrappedParam
       ) where

import qualified Network.HTTP.Types as HT
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.Maybe
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

-- | This 'Prism' convert from a 'ByteString' to 'Bool' value.
--
-- >>> booleanQuery # True
-- "true"
-- >>> booleanQuery # False
-- "false"
-- >>> "true" ^? booleanQuery
-- Just True
-- >>> "1" ^? booleanQuery
-- Just True
-- >>> "t" ^? booleanQuery
-- Just True
-- >>> "test" ^? booleanQuery
-- Just False
booleanQuery :: Prism' S.ByteString Bool
booleanQuery = prism' bs sb
  where
    bs True = "true"
    bs False = "false"
    sb "true" = Just True
    sb "1" = Just True
    sb "t" = Just True
    sb _ = Just False

-- | This 'Prism' convert from a 'ByteString' to the array of 'Integer' value.
--
-- This is not a valid Prism, for example:
--
-- @
-- "1, 2" ^? integerArrayQuery == Just [1,2]
-- integerArrayQuery # [1,2] != "1, 2"
-- @
--
-- >>> integerArrayQuery # [1]
-- "1"
-- >>> integerArrayQuery # [1,2234,3]
-- "1,2234,3"
-- >>> "1,2234,3" ^? integerArrayQuery
-- Just [1,2234,3]
-- >>> "" ^? integerArrayQuery
-- Just []
-- >>> "hoge,2" ^? integerArrayQuery
-- Nothing
integerArrayQuery :: Prism' S.ByteString [Integer]
integerArrayQuery = prism' bs_arr arr_bs
  where
    bs_arr xs = S8.intercalate "," $ xs ^.. traversed . re readShow
    arr_bs str = chkValid $ map (^? readShow) $ S8.split ',' str
    chkValid arr =
        if all isJust arr
        then Just (catMaybes arr)
        else Nothing

wrappedParam :: Parameters p => S.ByteString -> Prism' S.ByteString a -> Lens' p (Maybe a)
wrappedParam key aSBS = lens getter setter
   where
     getter = preview $ params . to (lookup key) . _Just . aSBS
     setter = flip (over params . replace key)
     replace k (Just v) = ((k, aSBS # v):) . dropAssoc k
     replace k Nothing = dropAssoc k
     dropAssoc k = filter ((/= k) . fst)
