module Web.Twitter.Enumerator.Utils
       ( enumLine
       , enumJSON
       , enumJsonToStatus
       , skipNothing
       , debugEE
       , fromJSON'
       )
       where

import Web.Twitter.Enumerator.Types

import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseMaybe)
import Data.Attoparsec (maybeResult, parse)
import Data.Attoparsec.Enumerator (iterParser)
import Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB
import Data.ByteString (ByteString)
import Data.Maybe

enumLine :: E.Enumeratee ByteString ByteString IO a
enumLine = EB.splitWhen newline
  where newline x = (x == 10) || (x == 13)

skipNothing :: Monad m => E.Enumeratee (Maybe a) a m r
skipNothing = EL.concatMap (\x -> [fromJust x | isJust x])

debugEE :: Show a => E.Enumeratee a a IO r
debugEE = EL.mapM $ \x -> (putStrLn . show) x >> return x

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = parseMaybe parseJSON

enumJSON :: Monad m => E.Enumeratee ByteString Value m a
enumJSON = E.sequence $ iterParser json

enumJsonToStatus :: Monad m => E.Enumeratee Value Status m a
enumJsonToStatus = EL.concatMap jts
  where jts v@(Array _) = fromJust . fromJSON' $ v
        jts v@(Object _) = maybe [] (:[]) $ fromJSON' v
        jts _ = []

