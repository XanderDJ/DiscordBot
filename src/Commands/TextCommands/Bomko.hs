module Commands.TextCommands.Bomko where

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Commands.Utility
import Commands.Types
import Text.Parsec
import Commands.Parsers
import Data.Text

dnCom :: Command
dnCom = Com "ldn (something)" (TextCommand dnCommand)

dnCommand :: Message -> DiscordHandler ()
dnCommand msg = do
    let dn = parse parseDN "parsing DN" (messageContent msg)
    sendMessage $ R.CreateMessage (messageChannelId msg) (append (extractRight dn) " deez nuts! HAH GOTTEM!")