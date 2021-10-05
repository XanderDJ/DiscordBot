module Commands.TextCommands.DamageCalc where

import Commands.Types
import Commands.Utility
import Discord
import Discord.Types
import qualified Discord.Requests as R
import Commands.Parsers

damageCalcCom :: Command
damageCalcCom = Com "ldc figure this out" (TextCommand dcCommand)


dcCommand :: Message -> DiscordHandler ()
dcCommand msg = do
    let (m, opts) = parseOptions (messageText msg)
    undefined