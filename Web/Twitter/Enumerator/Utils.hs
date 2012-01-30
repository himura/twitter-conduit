module Web.Twitter.Enumerator.Utils
       ( enumJSON
       , conduitParser
       , skipNothing
       , debugEE
       , fromJSON'
       )
       where

import Control.Monad.Trans
import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Attoparsec.Types as A
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import Data.ByteString (ByteString)
 
skipNothing :: Monad m => C.Conduit (Maybe a) m a
skipNothing = undefined -- EL.concatMap (\x -> [fromJust x | isJust x])

debugEE :: (MonadIO m, Show a) => C.Conduit a m a
debugEE = undefined -- EL.mapM $ \x -> (liftIO . putStrLn . show) x >> return x

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = parseMaybe parseJSON

enumJSON :: Monad m => C.Conduit ByteString m Value
enumJSON = undefined -- E.sequence $ iterParser json

conduitParser :: (CA.AttoparsecInput a, C.ResourceThrow m) => A.Parser a b -> C.Conduit a m b
conduitParser p =
  C.sequenceSink () $ \() -> do
    ret <- CA.sinkParser p
    return $ C.Emit () [ret]
