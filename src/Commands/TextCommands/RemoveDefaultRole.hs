module Commands.TextCommands.RemoveDefaultRole where


import Commands.Utility
import Commands.Types
import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.Maybe
import Data.Text (append)
import DiscordDB.Connection (getDbConnEnv)
import Control.Monad.Trans (lift)
import DiscordDB.Queries (removeGuildRole)
import Database.PostgreSQL.Simple (close)

removeDefCom :: Command
removeDefCom = Com "l(rdr|removedefaultrole) role1, role2" (TextCommand removeDefCommand)

removeDefCommand :: Message -> DiscordHandler ()
removeDefCommand m = do
    allowed <- checkAllowed m
    ifElse allowed (removeRole m) (sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", you do not have the permission to run this command."))

removeRole :: Message -> DiscordHandler ()
removeRole m = do
    -- first check for admin privileges
    let rolesMentioned = messageMentionRoles m
        guildId = messageGuildId m
    ifElse (isNothing guildId) (invalidGuild m) (removeRoles rolesMentioned (fromJust guildId) m)

removeRoles :: [Snowflake] -> Snowflake -> Message -> DiscordHandler ()
removeRoles [] _ m = sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", No roles were present in the message! you need to @role!")
removeRoles rIds gId m = do
    con <- lift getDbConnEnv
    if isNothing con
        then sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", couldn't connect to the database!")
        else do
            let guildRoles = map (makeGuildRole gId) rIds
            lift $ mapM_ (removeGuildRole (fromJust con)) guildRoles
            sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", default roles were removed!")
            lift $ close (fromJust con)