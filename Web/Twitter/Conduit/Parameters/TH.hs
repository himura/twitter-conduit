{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Twitter.Conduit.Parameters.TH
       ( HasParam
       , defineParam
       , defineParamBool
       , defineParamDay
       , defineParamInteger
       , defineParamIntegerArray
       , defineParamString
       , defineParamStringArray
       , wrappedParam
       ) where

import Control.Lens
import qualified Data.ByteString.Char8 as S8
import Data.Char
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Web.Twitter.Conduit.Parameters.Internal
import Web.Twitter.Conduit.Request
import Language.Haskell.TH

import Data.Proxy

wrappedParam :: (HasParam (key :: Symbol) supports, KnownSymbol key)
             => Proxy key
             -> (a -> PV)
             -> (PV -> a)
             -> Lens' (APIRequest supports responseType) (Maybe a)
wrappedParam proxy wrap unwrap = lens getter setter
  where
    key = S8.pack $ symbolVal proxy
    getter = preview $ params . to (lookup key) . _Just . to unwrap
    setter = flip (over params . replace key)
    replace k (Just v) = ((k, wrap v):) . dropAssoc k
    replace k Nothing = dropAssoc k
    dropAssoc k = filter ((/= k) . fst)

paramInteger
    :: (HasParam key supports, KnownSymbol key)
    => Proxy key
    -> Lens' (APIRequest supports responseType) (Maybe Integer)
paramInteger proxy = wrappedParam proxy PVInteger unPVInteger

paramBool
    :: (HasParam key supports, KnownSymbol key)
    => Proxy key
    -> Lens' (APIRequest supports responseType) (Maybe Bool)
paramBool proxy = wrappedParam proxy PVBool unPVBool

paramString
    :: (HasParam key supports, KnownSymbol key)
    => Proxy key
    -> Lens' (APIRequest supports responseType) (Maybe Text)
paramString proxy = wrappedParam proxy PVString unPVString

paramIntegerArray
    :: (HasParam key supports, KnownSymbol key)
    => Proxy key
    -> Lens' (APIRequest supports responseType) (Maybe [Integer])
paramIntegerArray proxy = wrappedParam proxy PVIntegerArray unPVIntegerArray

paramStringArray
    :: (HasParam key supports, KnownSymbol key)
    => Proxy key
    -> Lens' (APIRequest supports responseType) (Maybe [Text])
paramStringArray proxy = wrappedParam proxy PVStringArray unPVStringArray

paramDay
    :: (HasParam key supports, KnownSymbol key)
    => Proxy key
    -> Lens' (APIRequest supports responseType) (Maybe Day)
paramDay proxy = wrappedParam proxy PVDay unPVDay

snakeToLowerCamel :: String -> String
snakeToLowerCamel [] = []
snakeToLowerCamel "_" = []
snakeToLowerCamel ('_':x:xs) = toUpper x : snakeToLowerCamel xs
snakeToLowerCamel str = f ++ snakeToLowerCamel next
  where (f, next) = span (/= '_') str

defineParam :: TypeQ -> Name -> String -> Q [Dec]
defineParam typ paramf paramName = do
    sig <- sigD name [t| forall supports responseType. HasParam $tyLit supports => Lens' (APIRequest supports responseType) (Maybe $typ) |]
    body <- funD name [clause [] (normalB [| $(varE paramf) (Proxy :: Proxy $tyLit) |]) []]
    return [sig, body]
  where
    name = mkName $ snakeToLowerCamel paramName
    tyLit = litT $ strTyLit paramName

defineParamInteger :: String -> Q [Dec]
defineParamInteger = defineParam [t|Integer|] 'paramInteger

defineParamBool :: String -> Q [Dec]
defineParamBool = defineParam [t|Bool|] 'paramBool

defineParamString :: String -> Q [Dec]
defineParamString = defineParam [t|Text|] 'paramString

defineParamIntegerArray :: String -> Q [Dec]
defineParamIntegerArray = defineParam [t|[Integer]|] 'paramIntegerArray

defineParamStringArray :: String -> Q [Dec]
defineParamStringArray = defineParam [t|[String]|] 'paramStringArray

defineParamDay :: String -> Q [Dec]
defineParamDay = defineParam [t|Day|] 'paramDay
