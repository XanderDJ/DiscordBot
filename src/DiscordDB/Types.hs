module DiscordDB.Types where

import Data.Int

data GuildRoleT i = GuildRoleT {
    guildId :: i,
    roleId :: i
} deriving Show


type GuildRole = GuildRoleT Int64