module PokemonDB.Connection (getDbConn, getDbConnEnv) where

import Database.PostgreSQL.Simple
import GHC.Word (Word16)
import System.Environment
import qualified Data.ByteString.UTF8 as B
import Data.Maybe

type Host = String
type Port = Word16
type Database = String
type Username  = String
type Password = String

getDbConn :: Host -> Port -> Database -> Username -> Password -> IO Connection
getDbConn host port db login pass = connect ConnectInfo {
  connectHost = host,
  connectPort = port,
  connectDatabase = db,
  connectUser = login,
  connectPassword = pass
}


getDbConnEnv :: IO (Maybe Connection)
getDbConnEnv = do
  hostString <- lookupEnv "POSTGRES_CONN"
  if isJust hostString 
    then Just <$> connectPostgreSQL ((B.fromString . fromJust) hostString)
    else return Nothing

