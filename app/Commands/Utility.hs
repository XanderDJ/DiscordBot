module Commands.Utility where

import Data.Text 
import qualified Data.Text as T
import Discord ( restCall, DiscordHandler )
import qualified Discord.Requests as R
import Discord.Types ( Message(messageAuthor, messageChannel), User(userId) )
import Control.Monad ( void )
import qualified Data.Map as M

sendMessage :: R.ChannelRequest Message -> DiscordHandler ()
sendMessage = void . restCall

extractRight :: Either a b -> b
extractRight (Right b) = b
extractRight (Left a) = error "Tried extracting Right value from Left"

ifElse :: Bool -> DiscordHandler a -> DiscordHandler a -> DiscordHandler a
ifElse True a _ = a
ifElse False _ a = a

pingUserText :: Message -> Text
pingUserText m = pack $ "<@" ++ show (userId . messageAuthor $ m) ++ ">"

invalidMons :: Message -> [T.Text] -> DiscordHandler ()
invalidMons m lfts = sendMessage $ R.CreateMessage (messageChannel m) (T.append (pingUserText m) (T.append ", couldn't the following mons: " (T.intercalate "," lfts)))

noConnection :: Message -> DiscordHandler ()
noConnection m = sendMessage $ R.CreateMessage (messageChannel m) (T.append (pingUserText m) ", couldn't connect to the database!")

getOptionWithDefault :: Ord k => a -> [k] -> M.Map k a -> a
getOptionWithDefault def [] m = def
getOptionWithDefault def (k:ks) m = if M.member k m then m M.! k else getOptionWithDefault def ks m

toId :: Text -> Text
toId = T.replace " " "" . T.replace "-" "" . T.toLower