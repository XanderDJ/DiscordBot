module Commands.Auction.Nomination (nominationCommand) where

import Commands.Auction.Types
import Commands.Auction.Utility
  ( auctionActive,
    getParticipant,
    isParticipant,
    notEnoughSlots,
    storeAuctions,
    updateAuction,
    user,
  )
import Commands.Parsers (nominatePlayerP)
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (extractRight, ifElse, pingUserText, reportError)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad (void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Either (isLeft)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, append, pack, toLower, unpack)
import Discord (DiscordHandler, restCall)
import qualified Discord.Requests as R
import Discord.Types
import Text.Parsec (parse)

nominationCommand :: Command
nominationCommand = Com "lnom <player>" (AuctionCommand registerNomination)

registerNomination :: MVar [Auction] -> Message -> DiscordHandler ()
registerNomination = auctionActive canNominate

canNominate :: MVar [Auction] -> Message -> [Auction] -> Auction -> DiscordHandler ()
canNominate mvar m as a = ifElse (isJust $ _aCurrentBid a) (storeAuctions mvar as >> nominationAlreadyActive m) (isParticipant hasSlots mvar m as a)

hasSlots :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
hasSlots mvar m as a = do
  let part = getParticipant (user m) (_aParticipants a)
  ifElse (Prelude.length (_pTeam part) >= (fromJust . _aMaxAmountTeam) a) (storeAuctions mvar as >> notEnoughSlots m) (hasBudget mvar m as a)

hasBudget :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
hasBudget mvar m auctions auction = do
  let remainingBudget = fromJust . _pBudget $ getParticipant (user m) (_aParticipants auction)
  ifElse (remainingBudget < (fromJust . _aMinBid) auction) (storeAuctions mvar auctions >> reportError "You don't have the budget for this!" m) (parseNomination mvar m auctions auction)

parseNomination :: MVar [Auction] -> Message -> [Auction] -> Auction -> DiscordHandler ()
parseNomination mvar m as a = do
  let nomination = parse nominatePlayerP "parse Nominate player" (toLower (messageContent m))
  ifElse (isLeft nomination) (storeAuctions mvar as >> nominateCommandHelp m) (startNomination mvar m as a (extractRight nomination))

startNomination :: MVar Auctions -> Message -> Auctions -> Auction -> Text -> DiscordHandler ()
startNomination mvar m as a name = do
  let hasMentioned = (not . null) (messageMentions m)
      name' = if hasMentioned then (userName . head) (messageMentions m) else name
      currentBid = Just (user m, I name (_aMinBid a))
      a' = a {_aCurrentBid = currentBid}
      as' = updateAuction a' as
  lift $ putMVar mvar as'
  void . restCall $ R.CreateMessage (messageChannelId m) (append (pingUserText m) (nominateText name (_aMinBid a)))

nominateCommandHelp :: Message -> DiscordHandler ()
nominateCommandHelp m = void . restCall $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", correct usage: nom (player)")

nominationAlreadyActive :: Message -> DiscordHandler ()
nominationAlreadyActive m = void . restCall $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", there is already a nomination active!")

nominateText :: Text -> Maybe Int -> Text
nominateText t (Just x) = pack $ ", player " ++ unpack t ++ " was nominated! Starting bid is " ++ show x ++ " and held by the nominator!"
nominateText t Nothing = pack $ ", player " ++ unpack t ++ " was nominated!"
