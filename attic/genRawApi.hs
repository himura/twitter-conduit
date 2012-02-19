{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.HTTP.Conduit

import Text.XML.HXT.Core hiding (when)
import Text.XML.HXT.XPath

import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (ResourceT)

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.List
import Data.Char
import Data.Maybe
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad
import qualified Safe.Failure as SF

apiUrl :: String
apiUrl = "https://dev.twitter.com/docs/api"

parseHTML :: String -> IOStateArrow s b XmlTree
parseHTML = readString [ withValidate no
                       , withParseHTML yes
                       , withWarnings no
                       ]

getHTMLTree
  :: C.ResourceIO m
  => Request m
  -> Manager
  -> ResourceT m [XmlTree]
getHTMLTree req mgr = do
  res <- httpLbs req mgr
  let body = BL8.unpack . responseBody $ res
  liftIO $ runX $ parseHTML body

getXPText :: String -> XmlTree -> Maybe String
getXPText xpath = SF.head . getXPTexts xpath

getXPTexts :: String -> XmlTree -> [String]
getXPTexts xpath = runLA (getXPathTrees xpath >>> getText)

genApiName :: String -> (String, [String])
genApiName name = (apiName, args)
  where
    wa = wordArgs name
    apiName = lowerCamel $ map (lowerCamel . wordsBy (== '_') . fst) wa
    args = catMaybes $ map snd wa

wordArgs :: String -> [(String, Maybe String)]
wordArgs name = map convw $ wordsBy (== '/') name
  where
    convw wrd = case wrd of
      (':':str) -> (str, Just str)
      str -> (str, Nothing)

lowerCamel :: [[Char]] -> [Char]
lowerCamel (w:ws) = concat $ w : map toUpperStr ws
lowerCamel [] = []

toUpperStr :: [Char] -> [Char]
toUpperStr (c:cs) = toUpper c : cs
toUpperStr [] = []

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy f s =  case dropWhile f s of
  "" -> []
  s' -> w : wordsBy f s''
    where (w, s'') =
            break f s'

type ApiList = [(String, [ApiDecl])]
data Method = GET | POST | DELETE
            deriving (Show, Eq)
data ApiDecl =
  ApiDecl { apiMethod :: Method
          , resUrl :: String
          , functionName :: String
          , requiredArgs :: [String]
          , description :: String }

genHeader :: ApiList -> IO ()
genHeader desc = do
  T.putStr $ T.concat
    [ "module Web.Twitter.Enumerator.Raw\n"
    , "       (\n"
    ]
  forM_ desc $ \(hdr, apis) -> do
    putStrLn $ "       -- * " ++ hdr
    forM_ apis $ \api -> do
      putStrLn $ "       , " ++ functionName api
  T.putStr $ "       ) where\n"

genApi :: ApiList -> IO ()
genApi desc = do
  forM_ desc $ \(hdr, apis) -> do
    putStrLn $ "-- * " ++ hdr
    forM_ apis $ \ApiDecl{..} -> do
      let urlarg = genUrl True $ wordArgs resUrl
      putStrLn $ "-- ^ " ++ (concat $ intersperse "\n--   " $ lines description)
      putStrLn $ concat
        [ functionName
        , " :: "
        , concat $ take (length $ requiredArgs) $ repeat "String -> "
        , "HT.Query -> Enumerator Value TW a"
        ]
      putStrLn $ concat
        [ functionName
        , " "
        , concat $ intersperse " " $ map (++ "_") $ requiredArgs
        , " query = api"
        , show apiMethod
        , " ("
        , urlarg
        , ") query EL.head_"
        ]

genUrl :: Bool -> [(String, Maybe String)] -> String
genUrl beg ws =
  let (u,rst) = span (not . isJust . snd) ws
      u' = concat $ intersperse "/" $ map fst u 
      b = if beg then "\"" else "" in
  case rst of
    [] -> b ++ u' ++ "\""
    wws -> if null u'
           then genUrlArg wws
           else b ++ u' ++ "/\" ++ " ++ genUrlArg wws

genUrlArg :: [(String, Maybe String)] -> String
genUrlArg ws =
  let (u,rst) = span (isJust . snd) ws
      u' = concat $ intersperse " ++ \"/\" ++ " $ map ((++ "_") . fromJust . snd) u in
  case rst of
    [] -> u'
    wws -> u' ++ " ++ \"/" ++ genUrl False wws

main :: IO ()
main = do
  req <- parseUrl apiUrl
  trees <- withManager $ \mgr -> getHTMLTree req mgr
  tree <- SF.head trees
  let docs = runLA query tree
      apis = map (uncurry parseDeclAll) docs
  genHeader apis
  genApi apis

  where
    query = getXPathTrees "//div[contains(concat(' ', @class, ' '), ' api-docs ')]"
            >>> getXPathTrees "//table"
            >>> (getXPathTrees "//caption/strong/text()" >>> getText) &&& arrEachDef
    arrEachDef = arr . runLA $
                   getXPathTrees "//tbody/tr" >>>
                   ((getXPathTrees "//a/text()" >>> getText)
                    &&& (getXPathTrees "//td[2]/text()" >>> getText))
    parseDecl u desc =
      let (mstr:url:_) = words u
          met = case mstr of
            "GET" -> GET
            "POST" -> POST
            "DELETE" -> DELETE
            _ -> error $ "unknown method " ++ mstr
          (fn, args) = genApiName url
          desc' = dropWhile isSpace desc
      in ApiDecl met url fn args desc'
    parseDeclAll caption apis = (caption, map (uncurry parseDecl) apis)