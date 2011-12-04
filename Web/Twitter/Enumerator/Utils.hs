module Web.Twitter.Enumerator.Utils
       ( enumLine
       , enumJSON
       , skipNothing
       , debugEE
       , fromJSON'
       , catchErrorWithChunks
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
import Control.Exception
import Control.Monad.Trans

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

catchErrorWithChunks :: Monad m
                     => Iteratee a m b
                     -> (Stream a -> SomeException -> Iteratee a m b)
                     -> Iteratee a m b
catchErrorWithChunks i h = go i
  where
    go iter = Iteratee $ do
      step <- runIteratee iter
      case step of
        Yield _ _ -> return step
        Error err -> runIteratee (h (Chunks []) err)
        Continue k -> return (Continue (wrap k))
    wrap k EOF = Iteratee $ do
      res <- run (k EOF)
      case res of
        Left err -> runIteratee (enumEOF $$ h EOF err)
        Right b -> return (Yield b EOF)
    wrap k stream = Iteratee $ do
      step <- runIteratee (k stream)
      case step of
        Yield _ _ -> return step
        Error err -> do
          step' <- runIteratee (h stream err)
          case step' of
            Continue k' -> runIteratee (k' stream)
            _ -> return step'
        Continue k' -> return (Continue (wrap k'))
