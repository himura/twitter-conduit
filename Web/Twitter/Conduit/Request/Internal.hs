{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Twitter.Conduit.Request.Internal where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time.Calendar (Day)
import GHC.TypeLits (Symbol)
import GHC.Types (Constraint)
import qualified Network.HTTP.Types as HT

type family HasParam (key :: Symbol) (ks :: [Symbol]) :: Constraint where
    HasParam (key :: Symbol) (key ': ks) = ()
    HasParam (key :: Symbol) (k ': ks) = HasParam key ks

type APIQuery = [APIQueryItem]
type APIQueryItem = (ByteString, PV)

class Parameters a where
    params :: Lens' a APIQuery

data PV
    = PVInteger { unPVInteger :: Integer }
    | PVBool { unPVBool :: Bool }
    | PVString { unPVString :: Text }
    | PVIntegerArray { unPVIntegerArray :: [Integer] }
    | PVStringArray { unPVStringArray :: [Text] }
    | PVDay { unPVDay :: Day }
    deriving (Show, Eq)

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

