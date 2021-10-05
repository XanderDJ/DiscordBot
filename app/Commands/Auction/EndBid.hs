module Commands.Auction.EndBid (endBidCommand) where

import Commands.Auction.Types
  ( Auction (_aCurrentBid, _aParticipants),
    Auctions,
    Item (_iName, _iPrice),
    Participant (_pBudget, _pTeam),
    User (_uName),
  )
import Commands.Auction.Utility
  ( auctionActive,
    getParticipant,
    isAuctioneer,
    noNominationActive,
    storeAuctions,
    updateAuction,
    updateParticipants,
  )
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (ifElse)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad (void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Maybe (fromJust, isNothing)
import Data.Text (pack, unpack)
import Discord (DiscordHandler, restCall)
import qualified Discord.Requests as R
import Discord.Types (Message (messageChannel))

endBidCommand :: Command
endBidCommand = Com "lendbid" (AuctionCommand startEndBid)

startEndBid :: MVar [Auction] -> Message -> DiscordHandler ()
startEndBid mvar m = auctionActive mvar m canEndBid

canEndBid :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
canEndBid mvar m as a = do
  ifElse (isNothing $ _aCurrentBid a) (storeAuctions mvar as >> noNominationActive m) (isAuctioneer endBid mvar m as a)

endBid :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
endBid mvar m as a = do
  let Just (user, item) = _aCurrentBid a
      part = getParticipant user (_aParticipants a)
      part' = part {_pTeam = item : _pTeam part, _pBudget = Just $ (fromJust . _pBudget) part - (fromJust . _iPrice) item}
      a' = a {_aParticipants = updateParticipants part' (_aParticipants a), _aCurrentBid = Nothing}
      as' = updateAuction a' as
  lift $ putMVar mvar as'
  void . restCall $
    R.CreateMessage
      (messageChannel m)
      ( pack $
          unpack (_uName user) ++ " has won " ++ unpack (_iName item)
            ++ " for "
            ++ (show . fromJust . _iPrice) item
            ++ " currency and now has "
            ++ (show . fromJust . _pBudget) part'
            ++ " left!"
      )