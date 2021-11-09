module DiscordDB.Connection where

import Database.PostgreSQL.Simple
import qualified Data.ByteString.UTF8 as B
import System.Environment
import Data.Maybe

getDbConnEnv :: IO (Maybe Connection)
getDbConnEnv = do
  hostString <- lookupEnv "POSTGRES_ADM"
  if isJust hostString 
    then Just <$> connectPostgreSQL ((B.fromString . fromJust) hostString)
    else return Nothing