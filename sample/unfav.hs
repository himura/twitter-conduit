{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class
import Web.Twitter.Conduit
import System.Environment
import Common

main :: IO ()
main = withCF $ do
  [statusIdStr] <- liftIO getArgs
  let statusId = read statusIdStr
  targetStatus <- showId statusId []
  liftIO . putStrLn $ "Unfavorite Tweet: " ++ show targetStatus
  res <- favoritesDestroy statusId []
  liftIO $ print res