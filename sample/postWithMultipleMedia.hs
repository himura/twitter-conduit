{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Common

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Network.HTTP.Conduit
import System.Environment
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
    (status:filepathList) <- getArgs
    when (length filepathList > 4) $ do
        hPutStrLn stderr $ "You can upload upto 4 images in a single tweet, but we got " ++ show (length filepathList) ++ " images. abort."
        exitFailure
    twInfo <- getTWInfoFromEnv
    withManager $ \mgr -> do
        uploadedMediaList <- forM filepathList $ \filepath -> do
            liftIO . putStrLn $ "Upload media: " ++ filepath
            ret <- call twInfo mgr $ mediaUpload (MediaFromFile filepath)
            liftIO . putStrLn $ "Upload completed: media_id: " ++ ret ^. uploadedMediaId . to show ++ ", filepath: " ++ filepath
            return ret
        liftIO $ putStrLn $ "Post message: " ++ status
        res <- call twInfo mgr $ update (T.pack status) & mediaIds ?~ (uploadedMediaList ^.. traversed .  uploadedMediaId)
        liftIO $ print res
