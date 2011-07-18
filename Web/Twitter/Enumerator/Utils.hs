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
import Data.Attoparsec (maybeResult, parse)
import Data.Enumerator hiding (filter, map)
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB
import Data.ByteString (ByteString)
import Data.Maybe

enumLine :: Enumeratee ByteString ByteString IO a
enumLine = EB.splitWhen newline
  where newline x = (x == 10) || (x == 13)

skipNothing :: Monad m => Enumeratee (Maybe a) a m r
skipNothing = EL.concatMap (\x -> [fromJust x | isJust x])

debugEE :: Show a => Enumeratee a a IO r
debugEE = EL.mapM $ \x -> (putStrLn . show) x >> return x

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = resultToMaybe . fromJSON

resultToMaybe :: Data.Aeson.Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe _           = Nothing

enumJSON :: Monad m => Enumeratee ByteString (Maybe Value) m a
enumJSON = EL.map $ \line -> maybeResult $ parse json line

enumJsonToStatus :: Monad m => Enumeratee Value Status m a
enumJsonToStatus = EL.concatMap jts
  where jts v@(Array _) = filterJust $ map fromJSON' . fromJust . fromJSON' $ v
        jts v@(Object _) = filterJust $ [fromJSON' v]
        jts _ = []
        filterJust = map fromJust . filter isJust

