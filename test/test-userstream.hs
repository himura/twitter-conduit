import Web.Twitter.Enumerator
import Web.Twitter.Enumerator.Types

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Enumerator as A
import Data.Enumerator (Enumerator, ($=), (=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB

import qualified Data.Vector as V
import qualified Data.ByteString as B

import System.IO
import Data.Maybe

loadJson :: FilePath -> IO AE.Value
loadJson file = do
  h <- openFile file ReadMode
  c <- B.hGetContents h
  return . fromJust . A.maybeResult . A.parse AE.json $ c

enumJson :: FilePath -> Enumerator AE.Value IO b
enumJson file = do
  EB.enumFile file $= eeJson
    where eeJson = E.sequence $ A.iterParser AE.json

fromArray :: AE.Value -> Maybe AE.Array
fromArray (AE.Array x) = Just x
fromArray _ = Nothing

testArray :: AE.Value -> V.Vector (AE.Value, AE.Result StreamingAPI)
testArray j = V.map (\x -> (x, AE.fromJSON x :: AE.Result StreamingAPI)) $ fromJust . fromArray $ j

testArray2 :: AE.Value -> AE.Result [StreamingAPI]
testArray2 j = AE.fromJSON j
