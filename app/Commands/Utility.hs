module Commands.Utility where

import Data.Text ( pack, Text )
import Discord ( restCall, DiscordHandler )
import qualified Discord.Requests as R
import Discord.Types ( Message(messageAuthor), User(userId) )
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



getOptionWithDefault :: Ord k => a -> [k] -> M.Map k a -> a
getOptionWithDefault def [] m = def
getOptionWithDefault def (k:ks) m = if M.member k m then m M.! k else getOptionWithDefault def ks m