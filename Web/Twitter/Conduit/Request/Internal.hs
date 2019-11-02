{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Twitter.Conduit.Request.Internal where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time.Calendar (Day)
import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Types (Constraint)
import qualified Network.HTTP.Types as HT

data Param label t = label := t

type family HasParam (label :: Symbol) (paramType :: *) (params :: [Param Symbol *]) :: Constraint where
    HasParam label paramType ((label ':= paramType) ': ks) = ()
    HasParam label paramType ((label' ':= paramType') ': ks) = HasParam label paramType ks

type APIQuery = [APIQueryItem]
type APIQueryItem = (ByteString, PV)

data PV
    = PVInteger { unPVInteger :: Integer }
    | PVBool { unPVBool :: Bool }
    | PVString { unPVString :: Text }
    | PVIntegerArray { unPVIntegerArray :: [Integer] }
    | PVStringArray { unPVStringArray :: [Text] }
    | PVDay { unPVDay :: Day }
    deriving (Show, Eq)

class Parameters req where
    type SupportParameters req :: [Param Symbol *]
    params :: Lens' req APIQuery

class Parameter a where
    param ::
           Parameters p
        => ByteString -- ^ key
        -> Lens' p (Maybe a)
instance Parameter Integer where
    param key = rawParam key PVInteger unPVInteger
instance Parameter Bool where
    param key = rawParam key PVBool unPVBool
instance Parameter Text where
    param key = rawParam key PVString unPVString
instance Parameter [Integer] where
    param key = rawParam key PVIntegerArray unPVIntegerArray
instance Parameter [Text] where
    param key = rawParam key PVStringArray unPVStringArray
instance Parameter Day where
    param key = rawParam key PVDay unPVDay

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

rawParam ::
       Parameters p
    => ByteString -- ^ key
    -> (a -> PV) -- ^ wrap
    -> (PV -> a) -- ^ unwrap
    -> Lens' p (Maybe a)
rawParam key wrap unwrap = lens getter setter
   where
     getter = preview $ params . to (lookup key) . _Just . to unwrap
     setter = flip (over params . replace key)
     replace k (Just v) = ((k, wrap v):) . dropAssoc k
     replace k Nothing = dropAssoc k
     dropAssoc k = filter ((/= k) . fst)

instance ( Parameters req
         , Parameter a
         , KnownSymbol label
         , HasParam label a (SupportParameters req)
         , Functor f
         , lens ~ (((Maybe a) -> f (Maybe a)) -> req -> f req)) =>
         IsLabel label lens where
    fromLabel = param key
      where
        key = S8.pack (symbolVal (Proxy :: Proxy label))
