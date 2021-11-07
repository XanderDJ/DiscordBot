module Commands.TextCommands.Bomko where

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Commands.Utility
import Commands.Types
import Text.Parsec
import Commands.Parsers
import Data.Text

dnCom = Com "ldn (something)" (TextCommand dnCommand)

dnCommand :: Message -> DiscordHandler ()
dnCommand msg = do
    let dn = parse parseDN "parsing DN" (messageText msg)
    sendMessage $ R.CreateMessage (messageChannel msg) (append ((pack . show) dn) " deez nuts! HAH GOTTEM!")