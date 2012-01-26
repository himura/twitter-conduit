module Web.Twitter.Enumerator.Utils
       ( enumLine
       , enumJSON
       , skipNothing
       , debugEE
       , fromJSON'
       , toMaybeByteString
       , handleParseError
       )
       where

import Web.Twitter.Enumerator.Types

import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseMaybe)
import Data.Attoparsec.Enumerator (iterParser)
import Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
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

enumJSON :: Monad m => E.Enumeratee ByteString Value m a
enumJSON = E.sequence $ iterParser json

toMaybeByteString :: Show a => a -> Maybe ByteString
toMaybeByteString = Just . B8.pack . show

handleParseError :: Monad m => Iteratee ByteString m b -> Iteratee ByteString m b
handleParseError iter = iter `catchError` hndl
  where
    getChunk = continue return
    hndl e = getChunk >>= \x -> case x of
      Chunks xs -> throwError $ ParserException e xs
      _ -> throwError $ ParserException e []
