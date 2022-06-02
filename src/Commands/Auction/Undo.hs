{-# LANGUAGE RecordWildCards #-}
module Commands.Auction.Undo (undoCommand) where

import Commands.Auction.Types
  ( Auction (_aParticipants, _aPreviousBids, A),
    Auctions,
    Item (I),
    Participant (_pBudget, _pTeam),
    User,
  )
import Commands.Auction.Utility
  ( auctionActive,
    containsUser,
    getParticipant,
    isAuctioneer,
    isValidUser,
    nothingToUndo,
    storeAuctions,
    updateAuction,
    updateParticipants,
    userNotParticipating,
  )
import Commands.Parsers (undoP)
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (extractRight, ifElse, pingUserText, reportError)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad (void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Either (isLeft)
import Data.Text (append, pack, unpack)
import Discord (DiscordHandler, restCall)
import qualified Discord.Requests as R
import Discord.Types 
import Text.Parsec (parse)

undoCommand :: Command
undoCommand = Com "lundo - removes the last won auction from this user" (AuctionCommand undo')

undo' :: MVar Auctions -> Message -> DiscordHandler ()
undo' = auctionActive (isAuctioneer startUndo)

startUndo :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
startUndo mvar m as a = do
  ifElse (null (_aPreviousBids a)) (storeAuctions mvar as >> reportError "No bids to undo!" m) (undo mvar m as a)

undo :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
undo mvar m as a@A {..} = do
  let pb@(user, i) = head _aPreviousBids
      p = getParticipant user _aParticipants
      (item : items) = _pTeam p
      (I name (Just price)) = item
      (Just budget) = _pBudget p
      budget' = price + budget
      team' = items
      p' = p {_pBudget = Just budget', _pTeam = team'}
      ps = _aParticipants
      ps' = updateParticipants p' ps
      a' = a {_aParticipants = ps', _aPreviousBids = tail _aPreviousBids}
      as' = updateAuction a' as
  lift $ putMVar mvar as'
  void . restCall $ R.CreateMessage (messageChannelId m) (append (pingUserText m) (pack $ ", the previous bid has been undone: " ++ unpack name ++ " can now be nominated again!"))
