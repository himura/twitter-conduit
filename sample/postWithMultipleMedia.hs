{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Web.Twitter.Conduit
import System.Environment
import System.Exit (exitFailure)
import System.IO
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad
import Common

main :: IO ()
main = runTwitterFromEnv' $ do
    (status:filepathList) <- liftIO getArgs
    when (length filepathList > 4) $ liftIO $ do
        hPutStrLn stderr $ "You can upload upto 4 images in a single tweet, but we got " ++ show (length filepathList) ++ " images. abort."
        exitFailure
    uploadedMediaList <- forM filepathList $ \filepath -> do
        liftIO $ putStrLn $ "Upload media: " ++ filepath
        ret <- call $ mediaUpload (MediaFromFile filepath)
        liftIO $ putStrLn $ "Upload completed: media_id: " ++ ret ^. mediaId . to show ++ ", filepath: " ++ filepath
        return ret
    liftIO $ putStrLn $ "Post message: " ++ status
    res <- call $ update (T.pack status) & mediaIds ?~ (uploadedMediaList ^.. traversed .  mediaId)
    liftIO $ print res
