module Commands.TextCommands.AddDefaultRole where

import Commands.Utility
import Commands.Types
import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.Maybe (isNothing, fromJust)
import Data.Text ( append )
import Data.Int
import DiscordDB.Connection
import Control.Monad.Trans (lift)
import DiscordDB.Tables (GuildRoleF)
import DiscordDB.Types (GuildRoleT(GuildRoleT))
import DiscordDB.Queries (insertGuildRole)
import Database.PostgreSQL.Simple (close)

addDefRoleCom = Com "l(adr|adddefaultroles) role1, role2, ..." (TextCommand addDefRoleCommand)


addDefRoleCommand :: Message -> DiscordHandler ()
addDefRoleCommand m = do
    allowed <- checkAllowed m
    ifElse allowed (addRole m) (sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", you do not have the permission to run this command."))

addRole :: Message -> DiscordHandler ()
addRole m = do
    let rolesMentioned = messageMentionRoles m
        guildId = messageGuildId m
    ifElse (isNothing guildId) (invalidGuild m) (insertRoles rolesMentioned (fromJust guildId) m)


insertRoles :: [Snowflake] -> Snowflake -> Message -> DiscordHandler ()
insertRoles [] _ m = sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", you need to @roles! no roles found in the message!")
insertRoles roleIds guildId m = do
    con <- lift getDbConnEnv
    if isNothing con
        then sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", couldn't connect to the database!")
        else do
            let guildRoles = map (makeGuildRole guildId) roleIds
            lift $ mapM_ (insertGuildRole (fromJust con)) guildRoles
            sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", default roles were added!")
            lift $ close (fromJust con)


