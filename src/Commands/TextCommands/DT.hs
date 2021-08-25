module Commands.TextCommands.DT (dtCommand) where

import Commands.Utility
import Control.Monad
import Control.Monad.Trans
import Data.Either
import Data.Maybe
import Data.Text
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Parsers
import Pokemon.PokeApi
import Pokemon.Types
import Text.Parsec
import Commands.Types

dtCommand :: Command
dtCommand = Com "ldt (item/pokemon/move/nature/ability)" (TextCommand returnDT)

returnDT :: Message -> DiscordHandler ()
returnDT m = do
  let dt = parse dtP "parse DT" (toLower (messageText m))
  lift $ print dt
  ifElse (isLeft dt) (nonValidDt m) (handleDt (extractRight dt) m)

handleDt :: String -> Message -> DiscordHandler ()
handleDt dt m = do
  dt' <- lift $ getDt dt
  ifElse (isNothing dt') (dtNotFound m) (dtFound (fromJust dt') m)

dtFound :: DTType -> Message -> DiscordHandler ()
dtFound dt m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ ", " ++ show dt))

dtNotFound :: Message -> DiscordHandler ()
dtNotFound m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", couldn't find the data you were looking for.")

nonValidDt :: Message -> DiscordHandler ()
nonValidDt m = void . restCall $ R.CreateMessage (messageChannel m) "Usage: ldt (item/pokemon/move/nature/ability)"