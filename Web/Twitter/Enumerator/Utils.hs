{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Utils
       ( enumLine
       , enumJSON
       , skipNothing
       , debugEE
       , fromJSON'
       , fromJSONSearch'
       )
       where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseMaybe)
import Data.Attoparsec.Enumerator (iterParser)
import Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB
import Data.ByteString (ByteString)
import Data.Maybe
import Control.Monad.IO.Class (MonadIO (liftIO))

enumLine :: Monad m => E.Enumeratee ByteString ByteString m a
enumLine = EB.splitWhen newline
  where newline x = (x == 10) || (x == 13)

skipNothing :: Monad m => E.Enumeratee (Maybe a) a m r
skipNothing = EL.concatMap (\x -> [fromJust x | isJust x])

debugEE :: (MonadIO m, Show a) => E.Enumeratee a a m r
debugEE = EL.mapM $ \x -> (liftIO . putStrLn . show) x >> return x

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = parseMaybe parseJSON

fromJSONSearch' :: FromJSON a => Value -> Maybe a
fromJSONSearch' (Object o) = parseMaybe (.: "results") o
fromJSONSearch' _          = Nothing

enumJSON :: Monad m => E.Enumeratee ByteString Value m a
enumJSON = E.sequence $ iterParser json

