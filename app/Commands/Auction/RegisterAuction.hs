module Commands.Auction.RegisterAuction (registerAuctionCommand) where

import Commands.Auction.Types (Auction (A, _aName))
import Commands.Auction.Utility (auctionID, getAuction, user)
import Commands.Parsers (registerAuctionP)
import Commands.Types
  ( Command (..),
    CommandFunction (AuctionCommand),
  )
import Commands.Utility (extractRight, ifElse, pingUserText)
import Control.Concurrent.MVar
  ( MVar,
    putMVar,
    readMVar,
    takeMVar,
  )
import Control.Monad (void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import Data.Text (append, pack, toLower, unpack)
import Discord (DiscordHandler, restCall)
import qualified Discord.Requests as R
import Discord.Types (Message (messageChannel, messageText))
import Text.Parsec (parse)

registerAuctionCommand :: Command
registerAuctionCommand = Com "lhostauction <name> <minimum bid i.e. 5000> <minimum bid step i.e. 500> <amount of players per team>" (AuctionCommand registerAuction)

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

alreadyAnAuction :: Message -> DiscordHandler ()
alreadyAnAuction m = void $ restCall (R.CreateMessage (messageChannel m) (append (pingUserText m) ", there is already an auction registered in this channel!"))

auctionCommandHelp :: Message -> DiscordHandler ()
auctionCommandHelp m = void $ restCall (R.CreateMessage (messageChannel m) (append (pingUserText m) ", use the command in this way: lhostauction (name) (minimum bid i.e. 5000) (minimum bid step i.e. 500) (amount of players per team)"))

isValidAuction :: Auction -> Bool
isValidAuction (A _ _ (Just _) (Just _) (Just _) _ _ _ _) = True
isValidAuction _ = False