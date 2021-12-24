module Commands.Auction.RegisterParticipant (registerParticipantCommand) where

import Commands.Auction.Types
  ( Auction (_aAuctioneer, _aParticipants),
    Auctions,
    Participant (P, _pId),
    User (U, _uName),
  )
import Commands.Auction.Utility
  ( auctionActive,
    containsParticipant,
    notAuctioneer,
    updateAuction,
    user, storeAuctions
  )
import Commands.Parsers (parseIntCommand, registerParticipantP)
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (extractRight, ifElse, pingUserText, printIO, reportError)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad (void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Either (isLeft)
import Data.Maybe
import Data.Text (append, pack, toLower, unpack)
import qualified Data.Text as T
import Discord (DiscordHandler, restCall)
import qualified Discord.Requests as R
import Discord.Types
import Text.Parsec (parse)

registerParticipantCommand :: Command
registerParticipantCommand = Com "lrp <budget> @participant1 @participant2 ..." (AuctionCommand registerParticipant)

registerParticipant :: MVar [Auction] -> Message -> DiscordHandler ()
registerParticipant mvar m = auctionActive mvar m addParticipant

addParticipant :: MVar [Auction] -> Message -> [Auction] -> Auction -> DiscordHandler ()
addParticipant mvar m as a = do
  ifElse (user m /= _aAuctioneer a) (lift (putMVar mvar as) >> notAuctioneer m) (authorizedAddParticipant mvar m a as)

toParticipant :: Int -> Discord.Types.User -> Participant
toParticipant b u = P (U ((T.toLower . userName) u) (Just ((read . T.unpack . userDiscrim) u))) (Just b) []

authorizedAddParticipant :: MVar [Auction] -> Message -> Auction -> [Auction] -> DiscordHandler ()
authorizedAddParticipant mvar m a as = do
  let budget = parse (parseIntCommand "lrp") "" (messageText m)
  ifElse (isLeft budget || isNothing (extractRight budget)) (lift (putMVar mvar as) >> usageRp m) (checkParticipants mvar a as (fromJust (extractRight budget)) m)

usageRp = reportError "lrp <budget> @participant1 @participant2 ..."

checkParticipants :: MVar Auctions -> Auction -> Auctions -> Int -> Message -> DiscordHandler ()
checkParticipants mvar a as budget m = do
  let participants = (map (toParticipant budget) . messageMentions) m
  ifElse (null participants) (storeAuctions mvar as >> reportError "You have to mention the users that need to participate!" m) (checkDuplicateParticipants mvar a as participants m)

checkDuplicateParticipants :: MVar [Auction] -> Auction -> [Auction] -> [Participant] -> Message -> DiscordHandler ()
checkDuplicateParticipants mvar a as p m = do
  let registeredParticipants = _aParticipants a
      b = any (containsParticipant registeredParticipants) p
  ifElse b (storeAuctions mvar as >> participantAlreadyRegistered m) (addValidParticipant mvar a as p m)

addValidParticipant :: MVar [Auction] -> Auction -> [Auction] -> [Participant] -> Message -> DiscordHandler ()
addValidParticipant mvar a as p m = do
  let ps = _aParticipants a
      a' = a {_aParticipants = p ++ ps}
      as' = updateAuction a' as
  storeAuctions mvar as'
  void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", everyone was registered as a participant!")

participantAlreadyRegistered :: Message -> DiscordHandler ()
participantAlreadyRegistered m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", user already registered!")

participantCommandHelp :: Message -> DiscordHandler ()
participantCommandHelp m = void $ restCall (R.CreateMessage (messageChannel m) (append (pingUserText m) ", use the command in this way: rp (name)#(identifier) (budget i.e. 60000)"))

isValidParticipant :: Participant -> Bool
isValidParticipant (P _ (Just _) _) = True
isValidParticipant _ = False