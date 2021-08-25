module Commands.Auction.RegisterParticipant (registerParticipantCommand) where

import Commands.Auction.Imports
  ( Auction (_aAuctioneer, _aParticipants),
    DiscordHandler,
    MVar,
    Message (messageChannel, messageText),
    MonadTrans (lift),
    Participant (P, _pId),
    User (_uName),
    append,
    auctionActive,
    containsParticipant,
    isLeft,
    notAuctioneer,
    pack,
    parse,
    putMVar,
    registerParticipantP,
    restCall,
    toLower,
    unpack,
    updateAuction,
    user,
    void,
  )
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (extractRight, ifElse, pingUserText)
import qualified Discord.Requests as R

registerParticipantCommand :: Command
registerParticipantCommand = Com "lrp <discordname>#<identifier> <budget>" (AuctionCommand registerParticipant)

registerParticipant :: MVar [Auction] -> Message -> DiscordHandler ()
registerParticipant mvar m = auctionActive mvar m addParticipant

addParticipant :: MVar [Auction] -> Message -> [Auction] -> Auction -> DiscordHandler ()
addParticipant mvar m as a = do
  ifElse (user m /= _aAuctioneer a) (lift (putMVar mvar as) >> notAuctioneer m) (authorizedAddParticipant mvar m a as)

authorizedAddParticipant :: MVar [Auction] -> Message -> Auction -> [Auction] -> DiscordHandler ()
authorizedAddParticipant mvar m a as = do
  let part = parse registerParticipantP "parse Participant" (toLower (messageText m))
  ifElse (isLeft part || (not . isValidParticipant) (extractRight part)) (lift (putMVar mvar as) >> participantCommandHelp m) (checkDuplicateParticipant mvar m a as (extractRight part))

checkDuplicateParticipant :: MVar [Auction] -> Message -> Auction -> [Auction] -> Participant -> DiscordHandler ()
checkDuplicateParticipant mvar m a as p = do
  ifElse (containsParticipant p (_aParticipants a)) (lift (putMVar mvar as) >> participantAlreadyRegistered m) (addValidParticipant mvar m a as p)

addValidParticipant :: MVar [Auction] -> Message -> Auction -> [Auction] -> Participant -> DiscordHandler ()
addValidParticipant mvar m a as p = do
  let ps = _aParticipants a
      a' = a {_aParticipants = p : ps}
      as' = updateAuction a' as
  lift $ putMVar mvar as'
  void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ ", " ++ (unpack . _uName . _pId) p ++ " was registered as a participant!"))

participantAlreadyRegistered :: Message -> DiscordHandler ()
participantAlreadyRegistered m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", user already registered!")

participantCommandHelp :: Message -> DiscordHandler ()
participantCommandHelp m = void $ restCall (R.CreateMessage (messageChannel m) (append (pingUserText m) ", use the command in this way: rp (name)#(identifier) (budget i.e. 60000)"))

isValidParticipant :: Participant -> Bool
isValidParticipant (P _ (Just _) _) = True
isValidParticipant _ = False