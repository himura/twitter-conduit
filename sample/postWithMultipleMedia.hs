{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Twitter.Conduit
import qualified Web.Twitter.Conduit.Parameters as P
import Web.Twitter.Types.Lens
import Common

import Control.Lens
import Control.Monad
import qualified Data.Text as T
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
    mgr <- newManager tlsManagerSettings
    uploadedMediaList <- forM filepathList $ \filepath -> do
        putStrLn $ "Upload media: " ++ filepath
        ret <- call twInfo mgr $ mediaUpload (MediaFromFile filepath)
        putStrLn $ "Upload completed: media_id: " ++ ret ^. uploadedMediaId . to show ++ ", filepath: " ++ filepath
        return ret
    putStrLn $ "Post message: " ++ status
    res <- call twInfo mgr $ update (T.pack status) & P.mediaIds ?~ (uploadedMediaList ^.. traversed .  uploadedMediaId)
    print res
