{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import System.Process
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import System.FilePath
import System.Directory
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Network.HTTP.Conduit

import Web.Twitter.Conduit
import Common

iconPath :: IO FilePath
iconPath = (</> "icons") <$> confdir >>= ensureDirectoryExist

main :: IO ()
main = withCF $ do
  src <- userstream
  src C.$$+- CL.mapM_ (lift . lift . printTL)

showStatus :: Status -> T.Text
showStatus s = T.concat [ userScreenName . statusUser $ s
                        , ":"
                        , statusText s
                        ]

printTL :: StreamingAPI -> IO ()
printTL (SStatus s) = T.putStrLn . showStatus $ s
printTL (SRetweetedStatus s) = T.putStrLn $ T.concat [ userScreenName . rsUser $ s
                                                     , ": RT @"
                                                     , showStatus . rsRetweetedStatus $ s
                                                     ]
printTL (SEvent Event{..})
    | evEvent == "favorite" || evEvent == "unfavorite",
      Just (ETStatus st) <- evTargetObject = do
        let (from, fromIcon) = evUserInfo evSource
            (to, _toIcon) = evUserInfo evTarget
            evUserInfo (ETUser user) = (userScreenName user, userProfileImageURL user)
            evUserInfo _ = ("", Nothing)
            header = T.concat [ evEvent, "[", from, " -> ", to, "]"]
        T.putStrLn $ T.concat [ header, " :: ", showStatus st ]
        icon <- case fromIcon of
            Just iconUrl -> Just <$> fetchIcon (T.unpack from) (S8.unpack iconUrl)
            Nothing -> return Nothing
        notifySend header (showStatus st) icon
printTL s = print s

notifySend :: T.Text -> T.Text -> Maybe FilePath -> IO ()
notifySend header content icon = do
  let ic = maybe [] (\i -> ["-i", i]) icon
  void $ rawSystem "notify-send" $ [T.unpack header, T.unpack content] ++ ic

fetchIcon :: String -- ^ screen name
          -> String -- ^ icon url
          -> IO String
fetchIcon sn url = do
    ipath <- iconPath
    let fname = ipath </> sn ++ "__" ++ takeFileName url
    exists <- doesFileExist fname
    unless exists $ withManager $ \mgr -> do
        req <- liftIO $ parseUrl url
        res <- http req mgr
        responseBody res $$+- CB.sinkFile fname
    return fname
