{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Twitter.Conduit.Parameters.TH where

import Web.Twitter.Conduit.Parameters.Internal
import Language.Haskell.TH
import Control.Lens
import Data.Char

snakeToLowerCamel :: String -> String
snakeToLowerCamel [] = []
snakeToLowerCamel ('_':[]) = []
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

defineHasParamClass :: String -- ^ parameter name
                    -> Name -- ^ parameter type
                    -> Name -- ^ a Prism
                    -> Q [Dec]
defineHasParamClass paramName typeN prismN =
    defineHasParamClass' cNameS fNameS paramName typeN prismN
  where
    cNameS = paramNameToClassName paramName
    fNameS = snakeToLowerCamel paramName

defineHasParamClass' :: String -> String -> String -> Name -> Name -> Q [Dec]
defineHasParamClass' cNameS fNameS paramName typeN prismN = do
    a <- newName "a"
    cName <- newName cNameS
    fName <- newName fNameS
    let cCxt = cxt [classP ''Parameters [varT a]]
        tySig = sigD fName (appT (appT (conT ''Lens') (varT a)) (appT (conT ''Maybe) (conT typeN)))
        valDef = valD (varP fName) (normalB (appE (appE (varE 'wrappedParam) (litE (stringL paramName))) (varE prismN))) []
    dec <- classD cCxt cName [PlainTV a] [] [tySig, valDef]
    return [dec]

deriveHasParamInstances :: TypeQ -- ^ target type
                        -> [String] -- ^ parameter name
                        -> Q [Dec]
deriveHasParamInstances typ paramNameList = 
    mapM mkInstance cNameStrList
  where
    cNameStrList = map paramNameToClassName paramNameList
    mkInstance cn = instanceD (return []) (appT (conT (mkName cn)) typ) []
