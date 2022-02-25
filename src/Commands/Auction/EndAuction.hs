module Commands.Auction.EndAuction (endAuctionCommand) where

import Commands.Auction.Types
  ( Auction (_aCurrentBid, _aID, _aParticipants),
    Auctions,
  )
import Commands.Auction.Utility
  ( auctionActive,
    isAuctioneer,
    nominationStillActive,
    storeAuctions,
  )
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (ifElse, sendMessage)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad (void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Maybe (isJust)
import Data.Text (append, pack)
import Discord (DiscordHandler, restCall)
import qualified Discord.Requests as R
import Discord.Types 

endAuctionCommand :: Command
endAuctionCommand = Com "lendauction - ends the current auction in the channel" (AuctionCommand endAuction')

endAuction' :: MVar Auctions -> Message -> DiscordHandler ()
endAuction' mvar m = auctionActive mvar m startEndAuction

startEndAuction :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
startEndAuction = isAuctioneer canEndAuction

canEndAuction :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
canEndAuction mvar m as a = do
  ifElse (isJust $ _aCurrentBid a) (storeAuctions mvar as >> nominationStillActive m) (endAuction mvar m as a)

endAuction :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
endAuction mvar m as a = do
  let as' = Prelude.filter (\a' -> _aID a /= _aID a') as
      teams = _aParticipants a
  lift $ putMVar mvar as'
  sendMessage $ R.CreateMessage (messageChannelId m) "RESULTS:"
  sendTeams teams m

sendTeams [] _ = pure ()
sendTeams (t:ts) m = sendMessage (R.CreateMessage (messageChannelId m) ((pack . show) t)) >> sendTeams ts m

