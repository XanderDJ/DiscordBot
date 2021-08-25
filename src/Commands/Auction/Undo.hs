module Commands.Auction.Undo (undoCommand) where

import Commands.Auction.Imports
  ( Auction (_aParticipants),
    Auctions,
    DiscordHandler,
    Item (I),
    MVar,
    Message (messageChannel, messageText),
    MonadTrans (lift),
    Participant (_pBudget, _pTeam),
    User,
    append,
    auctionActive,
    containsUser,
    getParticipant,
    isAuctioneer,
    isLeft,
    isValidUser,
    nothingToUndo,
    pack,
    parse,
    putMVar,
    restCall,
    storeAuctions,
    undoP,
    unpack,
    updateAuction,
    updateParticipants,
    userNotParticipating,
    void,
  )
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (extractRight, ifElse, pingUserText)
import qualified Discord.Requests as R

undoCommand :: Command
undoCommand = Com "lundo <username>#<identifier> - removes the last won auction from this user" (AuctionCommand undo')

undo' :: MVar Auctions -> Message -> DiscordHandler ()
undo' mvar m = auctionActive mvar m (isAuctioneer startUndo)

startUndo :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
startUndo mvar m as a = do
  let u = parse undoP "parsing undo message" (messageText m)
  ifElse (isLeft u || (not . isValidUser . extractRight $ u)) (storeAuctions mvar as >> undoCommandHelp m) (canUndo mvar m as a (extractRight u))

canUndo :: MVar Auctions -> Message -> Auctions -> Auction -> User -> DiscordHandler ()
canUndo mvar m as a u = ifElse (containsUser u (_aParticipants a)) (canUndo2 mvar m as a u) (storeAuctions mvar as >> userNotParticipating m u)

canUndo2 :: MVar Auctions -> Message -> Auctions -> Auction -> User -> DiscordHandler ()
canUndo2 mvar m as a u = do
  let part = getParticipant u (_aParticipants a)
  ifElse (Prelude.null (_pTeam part)) (storeAuctions mvar as >> nothingToUndo m u) (undo mvar m as a part)

undo :: MVar Auctions -> Message -> Auctions -> Auction -> Participant -> DiscordHandler ()
undo mvar m as a p = do
  let (item : items) = _pTeam p
      (I name (Just price)) = item
      (Just budget) = _pBudget p
      budget' = price + budget
      team' = items
      p' = p {_pBudget = Just budget', _pTeam = team'}
      ps = _aParticipants a
      ps' = updateParticipants p' ps
      a' = a {_aParticipants = ps'}
      as' = updateAuction a' as
  lift $ putMVar mvar as'
  void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ ", the previous bid has been undone: " ++ unpack name ++ " can now be nominated again!"))

undoCommandHelp :: Message -> DiscordHandler ()
undoCommandHelp m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", correct usage: undo (name)#(identifier)")