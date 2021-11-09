{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module DiscordDB.Tables (GuildRoleF, selectGuildRole, guildRoleTable) where

import Opaleye
import DiscordDB.Types
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)


type GuildRoleF = GuildRoleT (Field SqlInt8)
$(makeAdaptorAndInstance "pGuildRole" ''GuildRoleT)

guildRoleTable :: Table GuildRoleF GuildRoleF
guildRoleTable = table "default_role" (pGuildRole GuildRoleT {
    guildId = tableField "guildid",
    roleId = tableField "roleid"
})

selectGuildRole :: Select GuildRoleF
selectGuildRole = selectTable guildRoleTable