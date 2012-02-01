import Control.Monad.Trans
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import Web.Twitter
import Common

main :: IO ()
main = withCF $ do
  C.runResourceT $ userstream C.$$ CL.mapM_ (lift . print)
