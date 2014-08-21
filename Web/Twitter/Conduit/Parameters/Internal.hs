{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Web.Twitter.Conduit.Parameters.Internal
       ( Parameters(..)
       , PV(..)
       , APIQuery
       , APIQueryItem
       , makeSimpleQuery
       , readShow
       ) where

import qualified Network.HTTP.Types as HT
import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time.Calendar (Day)
import Control.Lens

data PV
    = PVInteger { unPVInteger :: Integer }
    | PVBool { unPVBool :: Bool }
    | PVString { unPVString :: Text }
    | PVIntegerArray { unPVIntegerArray :: [Integer] }
    | PVStringArray { unPVStringArray :: [Text] }
    | PVDay { unPVDay :: Day }
    deriving (Show, Eq)

type APIQuery = [APIQueryItem]
type APIQueryItem = (ByteString, PV)

class Parameters a where
    params :: Lens' a APIQuery

makeSimpleQuery :: APIQuery -> HT.SimpleQuery
makeSimpleQuery = traversed . _2 %~ paramValueBS

paramValueBS :: PV -> ByteString
paramValueBS (PVInteger i) = S8.pack . show $ i
paramValueBS (PVBool True) = "true"
paramValueBS (PVBool False) = "false"
paramValueBS (PVString txt) = T.encodeUtf8 txt
paramValueBS (PVIntegerArray iarr) = S8.intercalate "," $ map (S8.pack . show) iarr
paramValueBS (PVStringArray iarr) = S8.intercalate "," $ map T.encodeUtf8 iarr
paramValueBS (PVDay day) = S8.pack . show $ day

-- | This 'Prism' convert from a 'ByteString' to some value based on 'Read' and 'Show'
--
-- >>> readShow # 2
-- "2"
-- >>> "1024" ^? readShow :: Maybe Integer
-- Just 1024
readShow :: (Read a, Show a) => Prism' ByteString a
readShow = prism' (S8.pack . show) (readMaybe . S8.unpack)
  where
    readMaybe str = case [x | (x, t) <- reads str, ("", "") <- lex t] of
        [x] -> Just x
        _ -> Nothing
