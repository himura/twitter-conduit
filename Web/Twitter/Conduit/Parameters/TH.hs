{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Twitter.Conduit.Parameters.TH where

import Web.Twitter.Conduit.Request
import Language.Haskell.TH
import Control.Lens
import qualified Data.ByteString as S
import Data.Char
import Data.Text (Text)
import Data.Time.Calendar (Day)

snakeToLowerCamel :: String -> String
snakeToLowerCamel [] = []
snakeToLowerCamel "_" = []
snakeToLowerCamel ('_':x:xs) = toUpper x : snakeToLowerCamel xs
snakeToLowerCamel str = f ++ snakeToLowerCamel next
  where (f, next) = span (/= '_') str

snakeToUpperCamel :: String -> String
snakeToUpperCamel = upcase . snakeToLowerCamel
  where
    upcase [] = []
    upcase (x:xs) = toUpper x : xs

paramNameToClassName :: String -> String
paramNameToClassName paramName = "Has" ++ snakeToUpperCamel paramName ++ "Param"

wrappedParam :: Parameters p
             => S.ByteString
             -> (a -> PV)
             -> (PV -> a)
             -> Lens' p (Maybe a)
wrappedParam key wrap unwrap = lens getter setter
   where
     getter = preview $ params . to (lookup key) . _Just . to unwrap
     setter = flip (over params . replace key)
     replace k (Just v) = ((k, wrap v):) . dropAssoc k
     replace k Nothing = dropAssoc k
     dropAssoc k = filter ((/= k) . fst)

defineHasParamClass :: Name -- wrap function
                    -> Name -- unwrap function
                    -> TypeQ -- wrapped type
                    -> String -- ^ parameter name
                    -> Q [Dec]
defineHasParamClass wrap unwrap typ paramName =
    defineHasParamClass' cNameS fNameS wrap unwrap typ paramName 
  where
    cNameS = paramNameToClassName paramName
    fNameS = snakeToLowerCamel paramName

defineHasParamClass' :: String -> String -> Name -> Name -> TypeQ -> String -> Q [Dec]
defineHasParamClass' cNameS fNameS wrap unwrap typ paramName = do
    a <- newName "a"
    cName <- newName cNameS
    fName <- newName fNameS
#if MIN_VERSION_template_haskell(2, 10, 0)
    let cCxt = cxt [conT ''Parameters `appT` varT a]
#else
    let cCxt = cxt [classP ''Parameters [varT a]]
#endif
    let tySig = sigD fName (appT (appT (conT ''Lens') (varT a)) (appT (conT ''Maybe) typ))
        valDef = valD (varP fName) (normalB (appE (appE (appE (varE 'wrappedParam) (litE (stringL paramName))) (conE wrap)) (varE unwrap))) []
    dec <- classD cCxt cName [PlainTV a] [] [tySig, valDef]
    return [dec]

deriveHasParamInstances :: Name -- ^ target data type name
                        -> [String] -- ^ parameter name
                        -> Q [Dec]
deriveHasParamInstances typName paramNameList =
    mapM mkInstance cNameStrList
  where
    cNameStrList = map paramNameToClassName paramNameList
    mkInstance cn = instanceD (return []) (appT (conT (mkName cn)) targetType) []
    targetType = do
        a <- newName "a"
        appT (appT (conT (mkName "APIRequest")) (conT typName)) (varT a)

defineHasParamClassInteger :: String -> Q [Dec]
defineHasParamClassInteger =
    defineHasParamClass 'PVInteger 'unPVInteger [t|Integer|]

defineHasParamClassBool :: String -> Q [Dec]
defineHasParamClassBool =
    defineHasParamClass 'PVBool 'unPVBool [t|Bool|]

defineHasParamClassString :: String -> Q [Dec]
defineHasParamClassString =
    defineHasParamClass 'PVString 'unPVString [t|Text|]

defineHasParamClassIntegerArray :: String -> Q [Dec]
defineHasParamClassIntegerArray =
    defineHasParamClass 'PVIntegerArray 'unPVIntegerArray [t|[Integer]|]

defineHasParamClassStringArray :: String -> Q [Dec]
defineHasParamClassStringArray =
    defineHasParamClass 'PVStringArray 'unPVStringArray [t|[Text]|]

defineHasParamClassDay :: String -> Q [Dec]
defineHasParamClassDay =
    defineHasParamClass 'PVDay 'unPVDay [t|Day|]
