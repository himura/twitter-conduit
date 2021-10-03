{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
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
import qualified Network.HTTP.Types as HT

data Param label t = label := t

type EmptyParams = ('[] :: [Param Symbol *])

type HasParam (label :: Symbol) (paramType :: *) (params :: [Param Symbol *]) = ParamType label params ~ paramType
type family ParamType (label :: Symbol) (params :: [Param Symbol *]) :: * where
    ParamType label ((label ':= paramType) ': ks) = paramType
    ParamType label ((label' ':= paramType') ': ks) = ParamType label ks

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

class ParameterValue a where
    wrap :: a -> PV
    unwrap :: PV -> a
instance ParameterValue Integer where
    wrap = PVInteger
    unwrap = unPVInteger
instance ParameterValue Bool where
    wrap = PVBool
    unwrap = unPVBool
instance ParameterValue Text where
    wrap = PVString
    unwrap = unPVString
instance ParameterValue [Integer] where
    wrap = PVIntegerArray
    unwrap = unPVIntegerArray
instance ParameterValue [Text] where
    wrap = PVStringArray
    unwrap = unPVStringArray
instance ParameterValue Day where
    wrap = PVDay
    unwrap = unPVDay

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
       (Parameters p, ParameterValue a)
    => ByteString -- ^ key
    -> Lens' p (Maybe a)
rawParam key = lens getter setter
   where
     getter = preview $ params . to (lookup key) . _Just . to unwrap
     setter = flip (over params . replace key)
     replace k (Just v) = ((k, wrap v):) . dropAssoc k
     replace k Nothing = dropAssoc k
     dropAssoc k = filter ((/= k) . fst)

instance ( Parameters req
         , ParameterValue a
         , KnownSymbol label
         , HasParam label a (SupportParameters req)
         , Functor f
         , lens ~ ((Maybe a -> f (Maybe a)) -> req -> f req)) =>
         IsLabel label lens where
#if MIN_VERSION_base(4, 10, 0)
    fromLabel = rawParam key
#else
    fromLabel _ = rawParam key
#endif
      where
        key = S8.pack (symbolVal (Proxy :: Proxy label))
