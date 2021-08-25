module Main where

import CommandMap
import Commands.Auction (Auctions)
import Commands.Types
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Data.Default (Default (def))
import Data.Either
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Text
import qualified Data.Text.IO as TIO
import Discord
  ( DiscordHandler,
    RunDiscordOpts (discordOnEvent, discordOnLog, discordToken),
    restCall,
    runDiscord,
  )
import qualified Discord.Requests as R
import Discord.Types
  ( Event (MessageCreate),
    Message (messageAuthor, messageText),
    User (userIsBot),
  )
import Parsers
import System.Environment (getArgs)
import Text.Parsec

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

eventHandler :: MVar Auctions -> Event -> DiscordHandler ()
eventHandler mvar event = case event of
  MessageCreate m -> if not (fromBot m) then runCommands mvar m commandMap else pure ()
  _ -> pure ()

runCommands :: MVar Auctions -> Message -> M.Map Text Command -> DiscordHandler ()
runCommands mvar m map = if isJust com then runCommand (fromJust com) mvar m map else pure ()
  where
    commandName = parse parseCommand "Parse Command name" ((toLower . messageText) m)
    com = if isLeft commandName then Nothing else M.lookup (fromRight "" commandName) map

runCommand :: Command -> MVar Auctions -> Message -> M.Map Text Command -> DiscordHandler ()
runCommand (Com _ (AuctionCommand f)) mvar m _ = f mvar m
runCommand (Com _ (HelpCommand f)) _ m map = f m map
runCommand (Com _ (TextCommand f)) _ m _ = f m
runCommand (Com _ NoOp) _ _ _ = pure ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
