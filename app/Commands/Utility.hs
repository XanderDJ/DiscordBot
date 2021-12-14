module Commands.Utility where

import Control.Monad.Trans
import qualified Data.Text as T hiding (map)
import Discord ( restCall, DiscordHandler )
import qualified Discord.Requests as R
import Discord.Types ( Message(messageAuthor, messageChannel, messageGuild), User(userId), Snowflake, Guild (guildId), memberRoles, RoleId, Role, rolePos, roleId, roleName )
import Control.Monad ( void )
import qualified Data.Map as M
import DiscordDB.Tables (GuildRoleF)
import Data.Maybe
import Data.Either
import DiscordDB.Types (GuildRoleT(GuildRoleT))
import Database.PostgreSQL.Simple (Connection)
import PokemonDB.Connection (getDbConnEnv)
import Text.Pretty.Simple (pPrint)

sendMessage :: R.ChannelRequest Message -> DiscordHandler ()
sendMessage = void . restCall

extractRight :: Either a b -> b
extractRight (Right b) = b
extractRight (Left a) = error "Tried extracting Right value from Left"

extractLeft :: Either a b -> a
extractLeft  (Left a) = a
extractLeft  (Right b) = error "Tried extracting Left from Right"

sideColor = Just 0xf984ef

ifElse :: Bool -> DiscordHandler a -> DiscordHandler a -> DiscordHandler a
ifElse True a _ = a
ifElse False _ a = a

pingUserText :: Message -> T.Text
pingUserText m = T.pack $ "<@" ++ show (userId . messageAuthor $ m) ++ ">"

reportError ::  T.Text -> Message -> DiscordHandler ()
reportError t m = sendMessage $ R.CreateMessage (messageChannel m) (T.append (pingUserText m) t)

invalidMons :: Message -> [T.Text] -> DiscordHandler ()
invalidMons m lfts = sendMessage $ R.CreateMessage (messageChannel m) (T.append (pingUserText m) (T.append ", couldn't the following mons: " (T.intercalate "," lfts)))

invalidGuild :: Message -> DiscordHandler ()
invalidGuild = reportError ", you need to send this command in a guild!"

noConnection :: Message -> DiscordHandler ()
noConnection = reportError ", couldn't connect to the database!"

getOptionWithDefault :: Ord k => a -> [k] -> M.Map k a -> a
getOptionWithDefault def [] m = def
getOptionWithDefault def (k:ks) m = if M.member k m then m M.! k else getOptionWithDefault def ks m


getOption :: Ord k => [k] -> M.Map k a -> Maybe a
getOption [] m = Nothing
getOption (k:ks) m = if k `M.member` m then Just (m M.! k) else getOption ks m

hasOption :: Ord k => [k] -> M.Map k a -> Bool
hasOption ks m = any (`elem` ks) (M.keys m)

toId :: T.Text -> T.Text
toId = T.replace " " "" . T.replace "-" "" . T.toLower

makeGuildRole :: Snowflake -> Snowflake -> GuildRoleF
makeGuildRole gId rId = GuildRoleT (fromIntegral gId) (fromIntegral rId)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

checkAllowed :: Message -> DiscordHandler Bool
checkAllowed m = do
    let mgId = messageGuild m
    if isNothing mgId
        then return False
        else do
            let gId = fromJust mgId
            allRoles <- restCall $ R.GetGuildRoles gId
            if isLeft allRoles
                then return False
                else do
                    let allRoles' = extractRight allRoles
                    guildMember <- restCall $ R.GetGuildMember gId (userId . messageAuthor $ m)
                    if isLeft guildMember
                        then return False
                        else do
                            let userRoles = memberRoles (extractRight guildMember)
                                userRolePos = rolePositions userRoles allRoles'
                                maximum' = Prelude.foldr1 (\x y ->if x >= y then x else y)
                                maxPos = maximum' userRolePos
                                botPos = findBotPos allRoles'
                            return (maxPos > botPos)


findBotPos :: [Role] -> Int
findBotPos [] = 0
findBotPos (r:rs) = if roleName r == "Lonewulfx6" then fromIntegral (rolePos r) else findBotPos rs

rolePositions :: [RoleId] -> [Role] -> [Int]
rolePositions rIds rs = map (findRolePos rs) rIds
 where
     findRolePos :: [Role] -> RoleId -> Int
     findRolePos [] rId = 0
     findRolePos (r:rs) rId = if roleId r == rId then fromIntegral (rolePos r) else findRolePos rs rId


pokemonDb :: (Connection -> Message -> DiscordHandler ()) -> Message -> DiscordHandler ()
pokemonDb f m = do
    con <- lift $ getDbConnEnv
    ifElse (isNothing con) (noConnection m) (f (fromJust con) m)

printIO :: Show a => a -> DiscordHandler ()
printIO a = lift $ pPrint a
