module Commands.Auction.Bid (bidCommand) where

import Commands.Auction.Types
import Commands.Auction.Utility
  ( auctionActive,
    getMaxBudget,
    getParticipant,
    isParticipant,
    noNominationActive,
    notEnoughSlots,
    storeAuctions,
    updateAuction,
    user,
  )
import Commands.Parsers (bidP)
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (extractRight, ifElse, pingUserText)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad (void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Either (isLeft)
import Data.Maybe (fromJust, isNothing)
import Data.Text (append, pack, toLower, unpack)
import Discord (DiscordHandler, restCall)
import qualified Discord.Requests as R
import Discord.Types 
import Text.Parsec (parse)

bidCommand :: Command
bidCommand = Com "lb  <x|x.y|xk|x.yk>" (AuctionCommand registerBid)

registerBid :: MVar Auctions -> Message -> DiscordHandler ()
registerBid = auctionActive canBid

canBid :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
canBid mvar m as a = do
  ifElse (isNothing $ _aCurrentBid a) (storeAuctions mvar as >> noNominationActive m) (isParticipant checkEnoughSlots mvar m as a)

checkEnoughSlots :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
checkEnoughSlots mvar m as a = do
  let part = getParticipant (user m) (_aParticipants a)
      teamSize = Prelude.length (_pTeam part)
  ifElse (teamSize >= (fromJust . _aMaxAmountTeam) a) (storeAuctions mvar as >> notEnoughSlots m) (parseBid mvar m as a)

parseBid :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
parseBid mvar m as a = do
  let bid = parse bidP "parse !bid command" (toLower (messageContent m))
  ifElse (isLeft bid || (isNothing . extractRight) bid) (storeAuctions mvar as >> bidCommandHelp m) (checkBidHigher mvar m as a ((fromJust . extractRight) bid))

checkBidHigher :: MVar Auctions -> Message -> Auctions -> Auction -> Int -> DiscordHandler ()
checkBidHigher mvar m as a b = do
  let (Just (_, I _ (Just currentBid))) = _aCurrentBid a
  ifElse (currentBid >= b) (storeAuctions mvar as >> mustBidHigher m currentBid) (checkBidCorrectStep mvar m as a b)

checkBidCorrectStep :: MVar Auctions -> Message -> Auctions -> Auction -> Int -> DiscordHandler ()
checkBidCorrectStep mvar m as a b = do
  let (Just step) = _aMinStep a
  ifElse (b `mod` step /= 0) (storeAuctions mvar as >> mustBidCorrectStep m step) (hasEnoughBudget mvar m as a b)

checkNotCurrentBidder :: MVar Auctions -> Message -> Auctions -> Auction -> Int -> DiscordHandler ()
checkNotCurrentBidder mvar m as a b = do
  let (Just (currentBidder, _)) = _aCurrentBid a
      u = user m
  ifElse (u == currentBidder) (storeAuctions mvar as >> alreadyHaveBid m) (hasEnoughBudget mvar m as a b)

hasEnoughBudget :: MVar Auctions -> Message -> Auctions -> Auction -> Int -> DiscordHandler ()
hasEnoughBudget mvar m as a b = do
  let maxBudget = getMaxBudget (user m) a
  ifElse (b > maxBudget) (storeAuctions mvar as >> notEnoughBudget m) (updateBid mvar m as a b)

updateBid :: MVar Auctions -> Message -> Auctions -> Auction -> Int -> DiscordHandler ()
updateBid mvar m as a b = do
  let (Just (_, I name oldBid)) = _aCurrentBid a
      newBid = Just (user m, I name (Just b))
      a' = a {_aCurrentBid = newBid}
      as' = updateAuction a' as
  lift $ putMVar mvar as'
  void . restCall $ R.CreateMessage (messageChannelId m) (append (pingUserText m) (pack $ " has successfully bid " ++ show b ++ " for " ++ unpack name ++ "!"))

bidCommandHelp :: Message -> DiscordHandler ()
bidCommandHelp m = void . restCall $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", correct usage: lb (x|x.y|xk|x.yk)")

notEnoughBudget :: Message -> DiscordHandler ()
notEnoughBudget m = void . restCall $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", you don't have the budget for this bid!")

alreadyHaveBid :: Message -> DiscordHandler ()
alreadyHaveBid m = void . restCall $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", you already have the current bid!")

mustBidHigher :: Message -> Int -> DiscordHandler ()
mustBidHigher m b = void . restCall $ R.CreateMessage (messageChannelId m) (append (pingUserText m) (pack $ ", you must bid higher than the previous bid (" ++ show b ++ ")!"))

mustBidCorrectStep :: Message -> Int -> DiscordHandler ()
mustBidCorrectStep m step = void . restCall $ R.CreateMessage (messageChannelId m) (append (pingUserText m) (pack $ ", your bid must be a multiple of " ++ show step ++ "!"))
