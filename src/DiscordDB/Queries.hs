module DiscordDB.Queries where

import Opaleye
import DiscordDB.Tables
import DiscordDB.Types

import Data.Int
import Database.PostgreSQL.Simple (Connection)
import Opaleye.Internal.Manipulation

insertGuildRole :: Connection -> GuildRoleF-> IO Int64
insertGuildRole con = runInsert con guildRoleTable 


deleteRole :: GuildRoleF -> Delete  Int64
deleteRole (GuildRoleT gId rId) = Delete {
    dTable = guildRoleTable,
    dWhere = \(GuildRoleT gId' rId') -> gId' .== gId .&& rId .== rId',
    dReturning = rCount
}

removeGuildRole :: Connection -> GuildRoleF -> IO Int64
removeGuildRole con gr = runDelete_ con (deleteRole gr)

selectDefaultRoles :: Int64 -> Select GuildRoleF
selectDefaultRoles gId = do
    guildRole <- selectGuildRole
    where_ $ guildId guildRole .== toFields gId
    return guildRole

getDefaultRoles :: Connection -> Int64 -> IO [GuildRole]
getDefaultRoles con gId = runSelect con $ selectDefaultRoles gId 