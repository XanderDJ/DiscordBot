module Commands.Types where

import BotState
import Commands.Auction
import Commands.CursorManager (CursorManager)
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Text
import Discord
import Discord.Types

type Usage = Text

type Options = M.Map Text Text

data CommandFunction
  = TextCommand (Message -> DiscordHandler ())
  | AuctionCommand (MVar Auctions -> Message -> DiscordHandler ())
  | HelpCommand (M.Map Text Command -> Message -> DiscordHandler ())
  | CursorCommand (MVar CursorManager -> Message -> DiscordHandler ())
  | StateCommand (BotState -> Message -> DiscordHandler ())
  | NoOp

data Command = Com Usage CommandFunction

instance Eq Command where
  (Com u1 _) == (Com u2 _) = u1 == u2
