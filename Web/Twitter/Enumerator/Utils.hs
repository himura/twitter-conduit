module Web.Twitter.Enumerator.Utils
       ( enumLine
       , enumJSON
       , skipNothing
       , debugEE
       , fromJSON'
       )
       where

import Control.Monad.Trans
import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Conduit as C
import Data.ByteString (ByteString)

enumLine :: Monad m => C.Conduit ByteString m ByteString
enumLine = undefined

skipNothing :: Monad m => C.Conduit (Maybe a) m a
skipNothing = undefined -- EL.concatMap (\x -> [fromJust x | isJust x])

debugEE :: (MonadIO m, Show a) => C.Conduit a m a
debugEE = undefined -- EL.mapM $ \x -> (liftIO . putStrLn . show) x >> return x

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = parseMaybe parseJSON

enumJSON :: Monad m => C.Conduit ByteString m Value
enumJSON = undefined -- E.sequence $ iterParser json
