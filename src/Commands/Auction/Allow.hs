module Commands.Auction.Allow where

import Commands.Auction.Types
import Commands.Auction.Utility
import Commands.Parsers
import Commands.Types
import Commands.Utility
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.Text as T
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Text.Parsec
import Text.Pretty.Simple

allowCom :: Command
allowCom = Com "lallow @person1 @person2 ..." (AuctionCommand allowParticipant)

allowParticipant :: MVar Auctions -> Message -> DiscordHandler ()
allowParticipant = auctionActive (isParticipant addParticipant)

addParticipant :: MVar Auctions -> Message -> [Auction] -> Auction -> DiscordHandler ()
addParticipant auctionMVar m oldAuctions currentAuction = do
  let participant = message2user m
      part = getParticipant participant (_aParticipants currentAuction)
      newUsers = map discordUser2User (messageMentions m)
  ifElse (null newUsers) (storeAuctions auctionMVar oldAuctions >> reportError (T.append (pingUserText m) ", you have to @ the participants you want to allow to bid for you!") m) $ do
    let part' = part {_pIds = newUsers ++ _pIds part}
        parts' = updateParticipants' part part' (_aParticipants currentAuction)
        auction' = currentAuction {_aParticipants = parts'}
        newAuctions = updateAuction auction' oldAuctions
    storeAuctions auctionMVar newAuctions
    sendMessage $ R.CreateMessage (messageChannelId m) (T.append (pingUserText m) ", mentioned persons have been allowed")
