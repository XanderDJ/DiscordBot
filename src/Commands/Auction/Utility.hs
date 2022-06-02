module Commands.Auction.Utility where

import Commands.Auction.Types
import Commands.Utility
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Data.Text
import Discord
import qualified Discord.Requests as R
import qualified Discord.Types as DT
import Text.Read (readMaybe)

getAuction :: AuctionID -> [Auction] -> Maybe Auction
getAuction _ [] = Nothing
getAuction id (a : as) = if id == _aID a then Just a else getAuction id as

getParticipant :: Commands.Auction.Types.User -> [Participant] -> Participant
getParticipant _ [] = error "No participant found with that user id"
getParticipant u (p : ps) = if u `elem` _pIds p then p else getParticipant u ps

auctionID :: DT.Message -> AuctionID
auctionID m = ID ((fromIntegral . fromJust) guildID) (fromIntegral channelID)
  where
    guildID = DT.messageGuildId m
    channelID = DT.messageChannelId m

user :: DT.Message -> Commands.Auction.Types.User
user m = U (toLower name) id
  where
    u = DT.messageAuthor m
    name = DT.userName u
    id = DT.userDiscrim u

auctionActive :: (MVar [Auction] -> DT.Message -> [Auction] -> Auction -> DiscordHandler ()) -> MVar [Auction] -> DT.Message -> DiscordHandler ()
auctionActive f mvar m  = do
  auctions <- lift $ takeMVar mvar
  let auction = getAuction (auctionID m) auctions
  ifElse (isNothing auction) (lift (putMVar mvar auctions) >> auctionNotFound m) (f mvar m auctions (fromJust auction))

auctionNotFound :: DT.Message -> DiscordHandler ()
auctionNotFound m = void $ restCall (R.CreateMessage (DT.messageChannelId m) (append (pingUserText m) ", There is no auction happening in this channel!"))

notAuctioneer :: DT.Message -> DiscordHandler ()
notAuctioneer m = void . restCall $ R.CreateMessage (DT.messageChannelId m) (append (pingUserText m) ", you're not the auctioneer of this auction!")

containsParticipant :: [Participant] -> Participant -> Bool
containsParticipant ps p = Data.List.foldr (\p' -> (||) (_pIds p == _pIds p')) False ps

containsUser :: Commands.Auction.Types.User -> [Participant] -> Bool
containsUser u = Data.List.foldr (\p' -> (||) (u `elem` _pIds p')) False

updateAuction :: Auction -> [Auction] -> [Auction]
updateAuction a [] = [a]
updateAuction a (a' : as) = if _aID a == _aID a' then a : as else a' : updateAuction a as

updateParticipants :: Participant -> [Participant] -> [Participant]
updateParticipants p [] = [p]
updateParticipants p (p' : ps) = if _pIds p == _pIds p' then p : ps else p' : updateParticipants p ps

updateParticipants' :: Participant -> Participant -> [Participant] -> [Participant]
updateParticipants' oldP newP [] = []
updateParticipants' oldP newP (p' : ps) = if _pIds oldP == _pIds p' then newP : ps else p' : updateParticipants' oldP newP ps

isParticipant :: (MVar Auctions -> DT.Message -> Auctions -> Auction -> DiscordHandler ()) -> MVar Auctions -> DT.Message -> Auctions -> Auction -> DiscordHandler ()
isParticipant f mvar m as a = do
  let uId = user m
  ifElse (containsUser uId (_aParticipants a)) (f mvar m as a) (storeAuctions mvar as >> notParticipating m)

notParticipating :: DT.Message -> DiscordHandler ()
notParticipating m = void . restCall $ R.CreateMessage (DT.messageChannelId m) (append (pingUserText m) ", you are not part of this auction!")

storeAuctions :: MVar Auctions -> Auctions -> DiscordHandler ()
storeAuctions mvar as = lift $ putMVar mvar as

notEnoughSlots :: DT.Message -> DiscordHandler ()
notEnoughSlots m = void . restCall $ R.CreateMessage (DT.messageChannelId m) (append (pingUserText m) ", your team is already full!")

isAuctioneer :: (MVar Auctions -> DT.Message -> Auctions -> Auction -> DiscordHandler ()) -> MVar Auctions -> DT.Message -> Auctions -> Auction -> DiscordHandler ()
isAuctioneer f mvar m as a = do
  ifElse (user m /= _aAuctioneer a) (storeAuctions mvar as >> notAuctioneer m) (f mvar m as a)

nothingToUndo :: DT.Message -> Commands.Auction.Types.User -> DiscordHandler ()
nothingToUndo m u = void . restCall $ R.CreateMessage (DT.messageChannelId m) (append (pingUserText m) (pack $ show u ++ " has nothing to undo!"))

nominationStillActive :: DT.Message -> DiscordHandler ()
nominationStillActive m = void . restCall $ R.CreateMessage (DT.messageChannelId m) (append (pingUserText m) ", there is a nomination still active!")

noNominationActive :: DT.Message -> DiscordHandler ()
noNominationActive m = void . restCall $ R.CreateMessage (DT.messageChannelId m) (append (pingUserText m) ", there is no nomination active!")

userNotParticipating :: DT.Message -> Commands.Auction.Types.User -> DiscordHandler ()
userNotParticipating m u = void . restCall $ R.CreateMessage (DT.messageChannelId m) (append (append (pingUserText m) (append ", " (pack (show u)))) " is not part of this auction!")

getMaxBudget :: Commands.Auction.Types.User -> Auction -> Int
getMaxBudget user auction = maxBudget
  where
    participant = getParticipant user (_aParticipants auction)
    itemsToBuy = (fromJust . _aAmountTeam) auction - (Prelude.length (_pTeam participant) + 1)
    maxBudget = (fromJust . _pBudget) participant - itemsToBuy * (fromJust . _aMinBid) auction

isValidUser :: Commands.Auction.Types.User -> Bool
isValidUser (U _ (Just _)) = True
isValidUser _ = False


message2user :: DT.Message -> User
message2user m = U name discrim
 where name = (toLower . DT.userName . DT.messageAuthor) m
       discrim = (DT.userDiscrim . DT.messageAuthor) m

discordUser2User :: DT.User -> User
discordUser2User u = U ((toLower . DT.userName) u) (DT.userDiscrim u)