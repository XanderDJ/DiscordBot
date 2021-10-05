module Commands.TextCommands.EmbedTest where

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Commands.Types
import Commands.Utility


embedCom = Com "testing embeds" (TextCommand embedMessage)


embedMessage :: Message -> DiscordHandler ()
embedMessage m = sendMessage $ R.CreateMessageEmbed (messageChannel m) "Test" embed
 where embed = undefined 