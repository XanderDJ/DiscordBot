module Main where

import Auction
    ( getAuction,
      Auction(..),
      AuctionID(..),
      Auctions,
      Item(I, _iName, _iPrice),
      Participant(..),
      User(U, _uName) )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, readMVar, takeMVar, MVar )
import Control.Monad ( void, when )
import Control.Monad.Trans ( MonadTrans(lift) )
import Data.Default ( Default(def) )
import Data.Either (fromRight, isLeft)
import Data.List (foldr)
import Data.Maybe ( fromJust, isJust, isNothing )
import Data.Text
    ( Text, append, isPrefixOf, pack, toLower, unpack )
import qualified Data.Text.IO as TIO
import Discord
    ( restCall,
      runDiscord,
      DiscordHandler,
      RunDiscordOpts(discordToken, discordOnLog, discordOnEvent) )
import Discord.Requests (AllowedMentions (mentionUsers))
import qualified Discord.Requests as R
import Discord.Types
  ( ChannelId,
    Event (MessageCreate),
    Message (..),
    User (userDiscrim, userId, userName, userIsBot),
  )
import GHC.ResponseFile (unescapeArgs)
import Parsers
    ( bidP,
      infoP,
      nominatePlayerP,
      registerAuctionP,
      registerParticipantP,
      undoP )
import System.Environment ( getArgs )
import Text.Parsec ( parse )

main = auctionBot

auctionBot :: IO ()
auctionBot = do
  [token] <- getArgs
  mVar <- newEmptyMVar
  putMVar mVar []
  userFacingError <-
    runDiscord $
      def
        { discordToken = append "Bot " (pack token),
          discordOnLog = print,
          discordOnEvent = eventHandler mVar
        }
  TIO.putStrLn userFacingError

eventHandler :: MVar [Auction] -> Event -> DiscordHandler ()
eventHandler mvar event = case event of
  MessageCreate m -> do
    when (isRegisterAuction m) (registerAuction mvar m)
    when (isRegisterParticipant m) (registerParticipant mvar m)
    when (isNomination m) (registerNomination mvar m)
    when (isBid m) (registerBid mvar m)
    when (isEndBid m) (startEndBid mvar m)
    when (isInfo m) (getInfo mvar m)
    when (isInfoUser m) (getInfoUser mvar m)
    when (isEndAuction m) (auctionActive mvar m startEndAuction)
    when (isHelp m) (help m)
    when (isUndo m) (auctionActive mvar m (isAuctioneer startUndo))
  _ -> pure ()

-- BOT ACTIONS

registerAuction :: MVar [Auction] -> Message -> DiscordHandler ()
registerAuction mvar m = do
  auctions <- lift $ readMVar mvar
  let aId = auctionID m
      auction = getAuction aId auctions
  ifElse (isNothing auction) (parseAuction mvar m) (alreadyAnAuction m)

parseAuction :: MVar [Auction] -> Message -> DiscordHandler ()
parseAuction mvar m = do
  let auction = parse (registerAuctionP (auctionID m) (user m)) "parse hostAuction" (toLower (messageText m))
      bool = not (isLeft auction) && isValidAuction (extractRight auction)
  ifElse bool (addValidAuction mvar m (extractRight auction)) (auctionCommandHelp m)

addValidAuction :: MVar [Auction] -> Message -> Auction -> DiscordHandler ()
addValidAuction mvar m a = do
  auctions <- lift $ takeMVar mvar
  let as' = a : auctions
  lift $ putMVar mvar as'
  void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ ", Auction " ++ (unpack . _aName) a ++ " was created!"))

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

registerBid :: MVar Auctions -> Message -> DiscordHandler ()
registerBid mvar m = auctionActive mvar m canBid

canBid :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
canBid mvar m as a = do
  ifElse (isNothing $ _aCurrentBid a) (storeAuctions mvar as >> noNominationActive m) (isParticipant mvar m as a checkEnoughSlots)

checkEnoughSlots :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
checkEnoughSlots mvar m as a = do
  let part = getParticipant (user m) (_aParticipants a)
      teamSize = Prelude.length (_pTeam part)
  ifElse (teamSize >= (fromJust . _aAmountTeam) a) (storeAuctions mvar as >> notEnoughSlots m) (parseBid mvar m as a)

parseBid :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
parseBid mvar m as a = do
  let bid = parse bidP "parse !bid command" (toLower (messageText m))
  ifElse (isLeft bid || (isNothing . extractRight) bid) (storeAuctions mvar as >> bidCommandHelp m) (checkBidHigher mvar m as a ((fromJust . extractRight) bid))

checkBidHigher :: MVar Auctions -> Message -> Auctions -> Auction -> Int -> DiscordHandler ()
checkBidHigher mvar m as a b = do
  let (Just (_, I _ (Just currentBid))) = _aCurrentBid a
  ifElse (currentBid >= b) (storeAuctions mvar as >> mustBidHigher m currentBid) (checkBidCorrectStep mvar m as a b)

checkBidCorrectStep :: MVar Auctions -> Message -> Auctions -> Auction -> Int -> DiscordHandler ()
checkBidCorrectStep mvar m as a b = do
  let (Just step) = _aMinStep a
  ifElse (b `mod` step /= 0) (storeAuctions mvar as >> mustBidCorrectStep m step) (checkNotCurrentBidder mvar m as a b)

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
  void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ " has successfully bid " ++ show b ++ " for " ++ unpack name ++ "!"))

startEndBid :: MVar [Auction] -> Message -> DiscordHandler ()
startEndBid mvar m = auctionActive mvar m canEndBid

canEndBid :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
canEndBid mvar m as a = do
  ifElse (isNothing $ _aCurrentBid a) (storeAuctions mvar as >> noNominationActive m) (isAuctioneer endBid mvar m as a )

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

startEndAuction :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
startEndAuction mvar m as a = isAuctioneer canEndAuction mvar m as a

canEndAuction :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
canEndAuction mvar m as a = do
  ifElse (isJust $ _aCurrentBid a) (storeAuctions mvar as >> nominationStillActive m) (endAuction mvar m as a)

endAuction :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
endAuction mvar m as a = do
  let as' = Prelude.filter (\a' -> _aID a /= _aID a') as
  lift $ putMVar mvar as'
  void . restCall $ R.CreateMessage (messageChannel m) (append "RESULTS:\n" ((pack . show) a))

getInfo :: MVar Auctions -> Message -> DiscordHandler ()
getInfo mvar m = do
  auctions <- lift $ readMVar mvar
  let auction = getAuction (auctionID m) auctions
  ifElse (isNothing auction) (auctionNotFound m) (checkParticipant mvar m auctions (fromJust auction) (user m))

checkParticipant :: MVar Auctions -> Message -> Auctions -> Auction -> Auction.User -> DiscordHandler ()
checkParticipant mvar m as a u = ifElse (containsUser (user m) (_aParticipants a)) (giveInfo mvar m as a u) (notParticipating m)

giveInfo :: MVar Auctions -> Message -> Auctions -> Auction -> Auction.User -> DiscordHandler ()
giveInfo mvar m as a u = do
  let p = getParticipant u (_aParticipants a)
  void . restCall $ R.CreateMessage (messageChannel m) (append (append (pingUserText m) ":\n") ((pack . show) p))

getInfoUser :: MVar Auctions -> Message -> DiscordHandler ()
getInfoUser mvar m = do
  auctions <- lift $ readMVar mvar
  let auction = getAuction (auctionID m) auctions
  ifElse (isNothing auction) (auctionNotFound m) (parseInfoUser mvar m auctions (fromJust auction))

parseInfoUser :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
parseInfoUser mvar m as a = do
  let u = parse infoP "parsing user info" (toLower (messageText m))
  ifElse (isLeft u) (infoUserCommandHelp m) (hasInfoUser mvar m as a (extractRight u))

hasInfoUser :: MVar Auctions -> Message -> Auctions -> Auction -> Auction.User -> DiscordHandler ()
hasInfoUser mvar m as a u = ifElse (containsUser u (_aParticipants a)) (giveInfoUser mvar m as a u) (userNotParticipating m u)

giveInfoUser :: MVar Auctions -> Message -> Auctions -> Auction -> Auction.User -> DiscordHandler ()
giveInfoUser mvar m as a u = do
  let p = getParticipant u (_aParticipants a)
  void . restCall $ R.CreateMessage (messageChannel m) (append (append (pingUserText m) ":\n") ((pack . show) p))

startUndo :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
startUndo mvar m as a = do
  let u = parse undoP "parsing undo message" (messageText m)
  ifElse (isLeft u || (not . isValidUser . extractRight $ u)) (storeAuctions mvar as >> undoCommandHelp m) (canUndo mvar m as a (extractRight u))

canUndo :: MVar Auctions -> Message -> Auctions -> Auction -> Auction.User -> DiscordHandler ()
canUndo mvar m as a u = ifElse (containsUser u (_aParticipants a)) (canUndo2 mvar m as a u) (storeAuctions mvar as >> userNotParticipating m u)

canUndo2 :: MVar Auctions -> Message -> Auctions -> Auction -> Auction.User -> DiscordHandler ()
canUndo2 mvar m as a u = do
  let part = getParticipant u (_aParticipants a)
  ifElse (Prelude.null (_pTeam part)) (storeAuctions mvar as >> nothingToUndo m u) (undo mvar m as a part)

undo :: MVar Auctions -> Message -> Auctions -> Auction -> Participant -> DiscordHandler ()
undo mvar m as a p = do
  let 
      (item:items) = _pTeam p
      (I name (Just price)) = item
      (Just budget) = _pBudget p
      budget' = price + budget
      team' = items
      p' = p {_pBudget = Just budget', _pTeam = team'}
      ps = _aParticipants a
      ps' = updateParticipants p' ps
      a' = a { _aParticipants = ps'}
      as' = updateAuction a' as
  lift $ putMVar mvar as'
  void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ ", the previous bid has been undone: " ++ unpack name ++ " can now be nominated again!"))

help :: Message -> DiscordHandler ()
help m =
  void . restCall $
    R.CreateMessage
      (messageChannel m)
      "!hostauction - starts an auction in the channel putting you as auctioneer\nrp - registerparticipant, register someone participating in the auction\nnom - nominate an object/player for bid\nb - bid for current nomination\ninfo - see your budget and items\ninfo (user) - see users budget and items\nendbid - end the current bid, person with last bid gets the item\nendauction - end the auction"

nominateText :: Text -> Maybe Int -> Text
nominateText t (Just x) = pack $ ", player " ++ unpack t ++ " was nominated! Starting bid is " ++ show x ++ " and held by the nominator!"
nominateText t Nothing = pack $ ", player " ++ unpack t ++ " was nominated!"

storeAuctions :: MVar Auctions -> Auctions -> DiscordHandler ()
storeAuctions mvar as = lift $ putMVar mvar as

auctionActive :: MVar [Auction] -> Message -> (MVar [Auction] -> Message -> [Auction] -> Auction -> DiscordHandler ()) -> DiscordHandler ()
auctionActive mvar m f = do
  auctions <- lift $ takeMVar mvar
  let auction = getAuction (auctionID m) auctions
  ifElse (isNothing auction) (lift (putMVar mvar auctions) >> auctionNotFound m) (f mvar m auctions (fromJust auction))

isParticipant :: MVar Auctions -> Message -> Auctions -> Auction -> (MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()) -> DiscordHandler ()
isParticipant mvar m as a f = do
  let uId = user m
  ifElse (containsUser uId (_aParticipants a)) (f mvar m as a) (storeAuctions mvar as >> notParticipating m)

isAuctioneer :: (MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()) -> MVar Auctions -> Message -> Auctions -> Auction ->  DiscordHandler ()
isAuctioneer f mvar m as a = do
  ifElse (user m /= _aAuctioneer a) (storeAuctions mvar as >> notAuctioneer m) (f mvar m as a)

nothingToUndo :: Message -> Auction.User -> DiscordHandler ()
nothingToUndo m u = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ show u ++ " has nothing to undo!"))

notEnoughSlots :: Message -> DiscordHandler ()
notEnoughSlots m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", your team is already full!")

notEnoughBudget :: Message -> DiscordHandler ()
notEnoughBudget m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", you don't have the budget for this bid!")

alreadyHaveBid :: Message -> DiscordHandler ()
alreadyHaveBid m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", you already have the current bid!")

mustBidHigher :: Message -> Int -> DiscordHandler ()
mustBidHigher m b = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ ", you must bid higher than the previous bid (" ++ show b ++ ")!"))

mustBidCorrectStep :: Message -> Int -> DiscordHandler ()
mustBidCorrectStep m step = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ ", your bid must be a multiple of " ++ show step ++ "!"))

noNominationActive :: Message -> DiscordHandler ()
noNominationActive m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", there is no nomination active!")

nominationAlreadyActive :: Message -> DiscordHandler ()
nominationAlreadyActive m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", there is already a nomination active!")

nominationStillActive :: Message -> DiscordHandler ()
nominationStillActive m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", there is a nomination still active!")

participantAlreadyRegistered :: Message -> DiscordHandler ()
participantAlreadyRegistered m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", user already registered!")

notParticipating :: Message -> DiscordHandler ()
notParticipating m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", you are not part of this auction!")

userNotParticipating :: Message -> Auction.User -> DiscordHandler ()
userNotParticipating m u = void . restCall $ R.CreateMessage (messageChannel m) (append (append (pingUserText m) (append ", " (pack (show u)))) " is not part of this auction!")

notAuctioneer :: Message -> DiscordHandler ()
notAuctioneer m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", you're not the auctioneer of this auction!")

participantCommandHelp :: Message -> DiscordHandler ()
participantCommandHelp m = void $ restCall (R.CreateMessage (messageChannel m) (append (pingUserText m) ", use the command in this way: rp (name)#(identifier) (budget i.e. 60000)"))

auctionCommandHelp :: Message -> DiscordHandler ()
auctionCommandHelp m = void $ restCall (R.CreateMessage (messageChannel m) (append (pingUserText m) ", use the command in this way: !hostauction (name) (minimum bid i.e. 5000) (minimum bid step i.e. 500) (amount of players per team)"))

nominateCommandHelp :: Message -> DiscordHandler ()
nominateCommandHelp m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", correct usage: nom (player)")

infoUserCommandHelp :: Message -> DiscordHandler ()
infoUserCommandHelp m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", correct usage: info (name)#(identifier)")

bidCommandHelp :: Message -> DiscordHandler ()
bidCommandHelp m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", correct usage: b (x|x.y|xk|x.yk)")

undoCommandHelp :: Message -> DiscordHandler ()
undoCommandHelp m = void . restCall $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", correct usage: undo (name)#(identifier)")

auctionNotFound :: Message -> DiscordHandler ()
auctionNotFound m = void $ restCall (R.CreateMessage (messageChannel m) (append (pingUserText m) ", There is no auction happening in this channel!"))

alreadyAnAuction :: Message -> DiscordHandler ()
alreadyAnAuction m = void $ restCall (R.CreateMessage (messageChannel m) (append (pingUserText m) ", there is already an auction registered in this channel!"))

-- HELPER FUNCTIONS

extractRight :: Either a b -> b
extractRight (Right b) = b
extractRight (Left a) = error "Tried extracting Right value from Left"

ifElse :: Bool -> DiscordHandler a -> DiscordHandler a -> DiscordHandler a
ifElse True a _ = a
ifElse False _ a = a

pingUserText :: Message -> Text
pingUserText m = pack $ "<@" ++ show (userId . messageAuthor $ m) ++ ">"

-- HELPER AUCTION FUNCTIONS

getMaxBudget :: Auction.User -> Auction -> Int
getMaxBudget user auction = maxBudget
  where
    participant = getParticipant user (_aParticipants auction)
    itemsToBuy = (fromJust . _aAmountTeam) auction - (Prelude.length (_pTeam participant) + 1)
    maxBudget = (fromJust . _pBudget) participant - itemsToBuy * (fromJust . _aMinBid) auction

getParticipant :: Auction.User -> [Participant] -> Participant
getParticipant _ [] = error "No participant found with that user id"
getParticipant u (p : ps) = if u == _pId p then p else getParticipant u ps

isValidAuction :: Auction -> Bool
isValidAuction (A _ _ (Just _) (Just _) (Just _) _ _ _) = True
isValidAuction _ = False

isValidParticipant :: Participant -> Bool
isValidParticipant (P _ (Just _) _) = True
isValidParticipant _ = False

isValidUser :: Auction.User -> Bool
isValidUser (U _ (Just _)) = True
isValidUser _ = False

lowText :: Message -> Text
lowText = toLower . messageText

isRegisterAuction :: Message -> Bool
isRegisterAuction m = "!hostauction" `isPrefixOf` lowText m && not (fromBot m)

isRegisterParticipant :: Message -> Bool
isRegisterParticipant m = "rp " `isPrefixOf` lowText m && not (fromBot m)

isBid :: Message -> Bool
isBid m = "b " `isPrefixOf` lowText m && not (fromBot m)

isNomination :: Message -> Bool
isNomination m = "nom " `isPrefixOf` lowText m && not (fromBot m)

isEndBid :: Message -> Bool
isEndBid m = "endbid" `isPrefixOf` lowText m && not (fromBot m)

isEndAuction :: Message -> Bool
isEndAuction m = "endauction" == lowText m && not (fromBot m)

isInfo :: Message -> Bool
isInfo m = "info" == lowText m && not (fromBot m)

isInfoUser :: Message -> Bool
isInfoUser m = "info " `isPrefixOf` lowText m && not (fromBot m)

isUndo :: Message -> Bool
isUndo m = "undo" `isPrefixOf` lowText m && not (fromBot m) 

isHelp :: Message -> Bool
isHelp m = "help" == lowText m && not (fromBot m)

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor 

auctionID :: Message -> AuctionID
auctionID m = ID ((fromIntegral . fromJust) guildID) (fromIntegral channelID)
  where
    guildID = messageGuild m
    channelID = messageChannel m

user :: Message -> Auction.User
user m = U (toLower name) id
  where
    user = messageAuthor m
    name = userName user
    id = Just . read . unpack . userDiscrim $ user

containsParticipant :: Participant -> [Participant] -> Bool
containsParticipant p = Data.List.foldr (\p' -> (||) (_pId p == _pId p')) False

containsUser :: Auction.User -> [Participant] -> Bool
containsUser u = Data.List.foldr (\p' -> (||) (u == _pId p')) False

updateAuction :: Auction -> [Auction] -> [Auction]
updateAuction a [] = [a]
updateAuction a (a' : as) = if _aID a == _aID a' then a : as else a' : updateAuction a as

updateParticipants :: Participant -> [Participant] -> [Participant]
updateParticipants p [] = [p]
updateParticipants p (p' : ps) = if _pId p == _pId p' then p : ps else p' : updateParticipants p ps