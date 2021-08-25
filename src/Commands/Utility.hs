module Commands.Utility where

import Data.Text ( pack, Text )
import Discord ( restCall, DiscordHandler )
import qualified Discord.Requests as R
import Discord.Types ( Message(messageAuthor), User(userId) )
import Control.Monad ( void )

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