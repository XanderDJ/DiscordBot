module Commands.Auction.EndAuction (endAuctionCommand) where

import Commands.Auction.Imports
  ( Auction (_aCurrentBid, _aID),
    Auctions,
    DiscordHandler,
    MVar,
    Message (messageChannel),
    MonadTrans (lift),
    append,
    auctionActive,
    isAuctioneer,
    isJust,
    nominationStillActive,
    pack,
    putMVar,
    restCall,
    storeAuctions,
    void,
  )
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (ifElse)
import qualified Discord.Requests as R

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
  lift $ putMVar mvar as'
  void . restCall $ R.CreateMessage (messageChannel m) (append "RESULTS:\n" ((pack . show) a))
