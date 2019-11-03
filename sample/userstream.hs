{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Common

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Conduit as HTTP
import System.Directory
import System.FilePath
import System.Process

ensureDirectoryExist :: FilePath -> IO FilePath
ensureDirectoryExist dir = do
    createDirectoryIfMissing True dir
    return dir

confdir :: IO FilePath
confdir = fmap (</> ".twitter-conduit") getHomeDirectory >>= ensureDirectoryExist

iconPath :: IO FilePath
iconPath = (</> "icons") <$> confdir >>= ensureDirectoryExist

main :: IO ()
main = do
    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings
    runResourceT $ do
        src <- stream twInfo mgr userstream
        runConduit $ src .| CL.mapM_ (liftIO . printTL)

showStatus :: AsStatus s => s -> T.Text
showStatus s = T.concat [ s ^. user . userScreenName
                        , ":"
                        , s ^. text
                        ]

printTL :: StreamingAPI -> IO ()
printTL (SStatus s) = T.putStrLn . showStatus $ s
printTL (SRetweetedStatus s) = T.putStrLn $ T.concat [ s ^. user . userScreenName
                                                     , ": RT @"
                                                     , showStatus (s ^. rsRetweetedStatus)
                                                     ]
printTL (SEvent event)
    | (event^.evEvent) == "favorite" || (event^.evEvent) == "unfavorite",
      Just (ETStatus st) <- event ^. evTargetObject = do
          let (fromUser, fromIcon) = evUserInfo (event^.evSource)
              (toUser, _toIcon) = evUserInfo (event^.evTarget)
              evUserInfo (ETUser u) = (u ^. userScreenName, u ^. userProfileImageURL)
              evUserInfo _ = ("", Nothing)
              header = T.concat [ event ^. evEvent, "[", fromUser, " -> ", toUser, "]"]
          T.putStrLn $ T.concat [ header, " :: ", showStatus st ]
          icon <- case fromIcon of
              Just iconUrl -> Just <$> fetchIcon (T.unpack fromUser) (T.unpack iconUrl)
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
    unless exists $ do
        req <- parseRequest url
        mgr <- newManager tlsManagerSettings
        runResourceT $ do
            body <- http req mgr
            runConduit $ HTTP.responseBody body .| CB.sinkFile fname
    return fname
