module Commands.Auction.Info (infoCommand, infoUserCommand) where

import Commands.Auction.Types
  ( Auction (_aParticipants),
    Auctions,
    User,
  )
import Commands.Auction.Utility
  ( auctionID,
    auctionNotFound,
    containsUser,
    getAuction,
    getParticipant,
    notParticipating,
    user,
    userNotParticipating,
  )
import Commands.Parsers (infoP)
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (extractRight, ifElse, pingUserText)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Monad (void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Either (isLeft)
import Data.Maybe (fromJust, isNothing)
import Data.Text (append, pack, toLower)
import Discord (DiscordHandler, restCall)
import qualified Discord.Requests as R
import Discord.Types hiding (User)
import Text.Parsec (parse)

infoCommand :: Command
infoCommand = Com "linfo - gives your current auction status" (AuctionCommand getInfo)

infoUserCommand :: Command
infoUserCommand = Com "linfouser <username>#<identifier> - gives users auction status" (AuctionCommand getInfoUser)

getInfo :: MVar Auctions -> Message -> DiscordHandler ()
getInfo mvar m = do
  auctions <- lift $ readMVar mvar
  let auction = getAuction (auctionID m) auctions
  lift $ print (user m)
  ifElse (isNothing auction) (auctionNotFound m) (checkParticipant mvar m auctions (fromJust auction) (user m))

checkParticipant :: MVar Auctions -> Message -> Auctions -> Auction -> User -> DiscordHandler ()
checkParticipant mvar m as a u = ifElse (containsUser (user m) (_aParticipants a)) (giveInfo mvar m as a u) (notParticipating m)

giveInfo :: MVar Auctions -> Message -> Auctions -> Auction -> User -> DiscordHandler ()
giveInfo mvar m as a u = do
  let p = getParticipant u (_aParticipants a)
  void . restCall $ R.CreateMessage (messageChannelId m) (append (append (pingUserText m) ":\n") ((pack . show) p))

getInfoUser :: MVar Auctions -> Message -> DiscordHandler ()
getInfoUser mvar m = do
  auctions <- lift $ readMVar mvar
  let auction = getAuction (auctionID m) auctions
  ifElse (isNothing auction) (auctionNotFound m) (parseInfoUser mvar m auctions (fromJust auction))

parseInfoUser :: MVar Auctions -> Message -> Auctions -> Auction -> DiscordHandler ()
parseInfoUser mvar m as a = do
  let u = parse infoP "parsing user info" (toLower (messageContent m))
  ifElse (isLeft u) (infoUserCommandHelp m) (hasInfoUser mvar m as a (extractRight u))

hasInfoUser :: MVar Auctions -> Message -> Auctions -> Auction -> User -> DiscordHandler ()
hasInfoUser mvar m as a u = ifElse (containsUser u (_aParticipants a)) (giveInfoUser mvar m as a u) (userNotParticipating m u)

giveInfoUser :: MVar Auctions -> Message -> Auctions -> Auction -> User -> DiscordHandler ()
giveInfoUser mvar m as a u = do
  let p = getParticipant u (_aParticipants a)
  void . restCall $ R.CreateMessage (messageChannelId m) (append (append (pingUserText m) ":\n") ((pack . show) p))

infoUserCommandHelp :: Message -> DiscordHandler ()
infoUserCommandHelp m = void . restCall $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", correct usage: info (name)#(identifier)")
