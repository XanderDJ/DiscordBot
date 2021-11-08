module Commands.TextCommands.DT (dtCommand) where

import Commands.Parsers
import Commands.Types
import Commands.Utility
import Control.Monad
import Control.Monad.Trans
import Data.Either
import Data.Maybe
import Data.Text
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Pokemon.Types
import PokemonDB.Connection (getDbConnEnv)
import qualified PokemonDB.Queries as Q
import PokemonDB.Types
import Pokemon.DBConversion
import Text.Parsec
import Database.PostgreSQL.Simple
import Pokemon.Nature

dtCommand :: Command
dtCommand = Com "ldt (item/pokemon/move/nature/ability)" (TextCommand returnDT)

returnDT :: Message -> DiscordHandler ()
returnDT m = do
  let dt = parse dtP "parse DT" (toLower (messageText m))
  ifElse (isLeft dt) (nonValidDt m) (handleDt (extractRight dt) m)

handleDt :: Text -> Message -> DiscordHandler ()
handleDt dt' m = do
  let 
    dt = toId dt'
    nature = getNature dt
  ifElse (isNothing nature) (handleDt' dt m) (sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) (append ", " (pack . show. fromJust $ nature))))


handleDt' :: Text -> Message -> DiscordHandler ()
handleDt' dt m = do
  con <- lift $ getDbConnEnv
  ifElse (isNothing con) (noConnection m) (handleDt'' (fromJust con) dt m)

handleDt'' :: Connection -> Text -> Message -> DiscordHandler ()
handleDt'' con dt m = do
  dt' <- lift $ Q.getData con dt
  lift $ close con
  ifElse (isNothing dt') (dtNotFound m) (dtFound (fromJust dt') m)

dtFound :: DBData -> Message -> DiscordHandler ()
dtFound dt m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ ", " ++ show (toDTType dt)))

dtNotFound :: Message -> DiscordHandler ()
dtNotFound m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", couldn't find the data you were looking for.")

nonValidDt :: Message -> DiscordHandler ()
nonValidDt m = void . restCall $ R.CreateMessage (messageChannel m) "Usage: ldt (item/pokemon/move/nature/ability)"