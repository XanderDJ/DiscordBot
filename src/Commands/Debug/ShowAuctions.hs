module Commands.Debug.ShowAuctions where

import Commands.Types
import BotState
import Discord.Types
import Discord
import Control.Monad.Trans
import Text.Pretty.Simple
import Control.Concurrent
import Commands.Auction.Types

showAuctionsCom :: Command
showAuctionsCom = Com "" (AuctionCommand showAuctions)

showAuctions :: MVar Auctions -> Message -> DiscordHandler ()
showAuctions auctionsMV m = do
 auctions <- lift $ readMVar auctionsMV
 pPrint auctions