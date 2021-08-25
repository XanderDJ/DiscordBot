module Commands.Auction.EndBid (endBidCommand) where

import Commands.Auction.Imports
  ( Auction (_aCurrentBid, _aParticipants),
    Auctions,
    DiscordHandler,
    Item (_iName, _iPrice),
    MVar,
    Message (messageChannel),
    MonadTrans (lift),
    Participant (_pBudget, _pTeam),
    User (_uName),
    auctionActive,
    fromJust,
    getParticipant,
    isAuctioneer,
    isNothing,
    noNominationActive,
    pack,
    putMVar,
    restCall,
    storeAuctions,
    unpack,
    updateAuction,
    updateParticipants,
    void,
  )
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (ifElse)
import qualified Discord.Requests as R

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
