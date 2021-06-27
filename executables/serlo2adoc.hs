import System.Environment (getArgs)
import System.Exit (die)
import Data.String (fromString)
import Data.Aeson (eitherDecodeFileStrict)
import Text.PrettyPrint.HughesPJ (render)

import Serlo.Model (SerloContent)
import Serlo.Encode
import Text.AsciiDoc.ToAsciiDoc (encodeAsAsciiDoc)

--decodeSerlo :: String -> Either String SerloContent
--decodeSerlo = eitherDecode . fromString

encodeSerlo :: SerloContent -> String
encodeSerlo = render . encodeAsAsciiDoc

--convert :: String -> String
--convert s = (either id id) $ encodeSerlo <$> decodeSerlo s

main = do
  as <- getArgs
  case as of
    [] -> die "Usage: serlo2adoc <file name>"
    (f:_) -> do
      sm <- eitherDecodeFileStrict f :: IO (Either String SerloContent)
      case sm of
        Left s -> die s
        Right c -> putStrLn $ encodeSerlo c
