{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
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

type family HasParam (label :: Symbol) (paramType :: t) (params :: [Param Symbol t]) :: Constraint where
    HasParam label paramType ((label ':= paramType) ': ks) = ()
    HasParam label paramType ((label' ':= paramType') ': ks) = HasParam label paramType ks

type APIQuery = [APIQueryItem]
type APIQueryItem = (ByteString, PV)

class Parameters req where
    type SupportedParams req :: [Param Symbol t]
    params :: Lens' req APIQuery

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

wrappedParam :: (Parameters p, KnownSymbol label)
             => Proxy label
             -> (a -> PV)
             -> (PV -> a)
             -> Lens' p (Maybe a)
wrappedParam proxy wrap unwrap = lens getter setter
   where
     key = S8.pack $ symbolVal proxy
     getter = preview $ params . to (lookup key) . _Just . to unwrap
     setter = flip (over params . replace key)
     replace k (Just v) = ((k, wrap v):) . dropAssoc k
     replace k Nothing = dropAssoc k
     dropAssoc k = filter ((/= k) . fst)

class (Parameters req, HasParam label a (SupportedParams req)) => Parameter (label :: Symbol) req a | label -> a where
    lParam :: Lens' req (Maybe a)

instance (Parameter label req a, Parameters req, HasParam label a (SupportedParams req), Functor f, lens ~ (((Maybe a) -> f (Maybe a)) -> req -> f req)) => IsLabel label lens where
    fromLabel = lParam @ label
