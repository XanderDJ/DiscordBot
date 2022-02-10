module Commands.Types where

import Commands.Auction
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Text
import Discord
import Discord.Types

type Usage = Text

data CommandFunction
  = TextCommand (Message -> DiscordHandler ())
  | AuctionCommand (MVar Auctions -> Message -> DiscordHandler ())
  | HelpCommand (Message -> M.Map Text Command -> DiscordHandler ())
  | NoOp 

data Command = Com Usage CommandFunction


instance Eq Command where
  (Com u1 _) == (Com u2 _) = u1 == u2
