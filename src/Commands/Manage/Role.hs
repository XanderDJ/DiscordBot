module Commands.Manage.Role where


import Discord.Types
import Discord
import qualified Discord.Requests as R
import Control.Monad.Trans
import Commands.Types
import Commands.Utility
import Control.Monad (void)
import DiscordDB.Connection (getDbConnEnv)
import DiscordDB.Queries
import Data.Maybe
import DiscordDB.Types (GuildRoleT(roleId))
import Database.PostgreSQL.Simple (close)

addRoleToUser :: GuildId -> GuildMember ->  DiscordHandler ()
addRoleToUser gId gMember = do 
    con <- lift getDbConnEnv
    if isNothing con
     then pure ()
     else do
        defaultRoles <- lift $ getDefaultRoles (fromJust con) (fromIntegral gId)
        let roleIds = map DiscordDB.Types.roleId defaultRoles
        if isNothing (memberUser gMember)
            then pure ()
            else mapM_ (addRole gId (userId . fromJust . memberUser $ gMember)) roleIds
        lift $ close (fromJust con)

addRole :: Integral a => GuildId -> UserId -> a -> DiscordHandler (Either RestCallErrorCode ())
addRole gId uId rId = restCall $ R.AddGuildMemberRole gId uId (fromIntegral rId)