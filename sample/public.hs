{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans
import Data.Default

import Web.Twitter.Conduit

main :: IO ()
main = runTW (def :: TWInfo NoToken) $ do
  publicTimeline [] C.$= CL.isolate 100 C.$$ CL.mapM_ $ \ret -> liftIO $ print ret
