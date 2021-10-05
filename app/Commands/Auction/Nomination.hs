module Commands.Auction.Nomination (nominationCommand) where

import Commands.Auction.Types
  ( Auction (_aAmountTeam, _aCurrentBid, _aMinBid, _aParticipants),
    Auctions,
    Item (I),
    Participant (_pTeam),
  )
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
import Commands.Utility (extractRight, ifElse, pingUserText)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad (void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Either (isLeft)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, append, pack, toLower, unpack)
import Discord (DiscordHandler, restCall)
import qualified Discord.Requests as R
import Discord.Types (Message (messageChannel, messageText))
import Text.Parsec (parse)

nominationCommand :: Command
nominationCommand = Com "lnom <player>" (AuctionCommand registerNomination)

registerNomination :: MVar [Auction] -> Message -> DiscordHandler ()
registerNomination mvar m = auctionActive mvar m canNominate

canNominate :: MVar [Auction] -> Message -> [Auction] -> Auction -> DiscordHandler ()
canNominate mvar m as a = ifElse (isJust $ _aCurrentBid a) (storeAuctions mvar as >> nominationAlreadyActive m) (isParticipant mvar m as a hasSlots)

hasSlots :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
hasSlots mvar m as a = do
  let part = getParticipant (user m) (_aParticipants a)
  ifElse (Prelude.length (_pTeam part) >= (fromJust . _aAmountTeam) a) (storeAuctions mvar as >> notEnoughSlots m) (parseNomination mvar m as a)

parseNomination :: MVar [Auction] -> Message -> [Auction] -> Auction -> DiscordHandler ()
parseNomination mvar m as a = do
  let nomination = parse nominatePlayerP "parse Nominate player" (toLower (messageText m))
  ifElse (isLeft nomination) (storeAuctions mvar as >> nominateCommandHelp m) (startNomination mvar m as a (extractRight nomination))

startNomination :: MVar Auctions -> Message -> Auctions -> Auction -> Text -> DiscordHandler ()
startNomination mvar m as a name = do
  let currentBid = Just (user m, I name (_aMinBid a))
      a' = a {_aCurrentBid = currentBid}
      as' = updateAuction a' as
  lift $ putMVar mvar as'
  void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) (nominateText name (_aMinBid a)))

nominateCommandHelp :: Message -> DiscordHandler ()
nominateCommandHelp m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", correct usage: nom (player)")

nominationAlreadyActive :: Message -> DiscordHandler ()
nominationAlreadyActive m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", there is already a nomination active!")

nominateText :: Text -> Maybe Int -> Text
nominateText t (Just x) = pack $ ", player " ++ unpack t ++ " was nominated! Starting bid is " ++ show x ++ " and held by the nominator!"
nominateText t Nothing = pack $ ", player " ++ unpack t ++ " was nominated!"