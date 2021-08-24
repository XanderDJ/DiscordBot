module Pokemon.Replays.API where

import Control.Exception
import qualified Data.ByteString.Lazy as B
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Pokemon.Replays.Parsers
import Pokemon.Replays.Types
import Text.Parsec
import Data.Either

settings = tlsManagerSettings

getReplay :: String -> IO (Maybe [ReplayMessage])
getReplay path = do
  manager <- newManager settings
  response <- catch (httpLbs (parseRequest_ (path ++ ".log")) manager <&> Just) (\(HttpExceptionRequest req content) -> return Nothing)
  if isJust response
    then do
      let (Just txt) = T.lines . decodeLatin1 . B.toStrict . responseBody <$> response
      return $ Just $ (rights . map (parse parseReplayMessage "Parser")) txt
    else do
      return Nothing

