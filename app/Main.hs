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
    RunDiscordOpts (discordOnEvent, discordOnLog, discordToken, discordOnEnd, discordOnStart),
    restCall,
    runDiscord,
  )
import qualified Discord.Requests as R
import Discord.Types
  ( Event (MessageCreate, GuildMemberUpdate, GuildMemberAdd, InteractionCreate),
    Message (messageAuthor, messageContent, messageId),
    User (userIsBot),
  )
import Commands.Parsers
import System.Environment (getArgs)
import Text.Parsec
import qualified Discord
import qualified Discord.Requests as R
import Control.Monad
import Control.Monad.Trans
import Commands.Manage.Role
import Discord.Interactions
import Text.Pretty.Simple (pPrint)
import System.Random (getStdGen)
import BotState (BotState(BotState))

main = bot

bot :: IO ()
bot = do
  [token] <- getArgs
  auctionVar <- newEmptyMVar
  putMVar auctionVar []
  botStateVar <- newEmptyMVar
  stdGen <- getStdGen
  putMVar botStateVar (BotState stdGen)
  userFacingError <-
    runDiscord $
      def
        { discordToken = append "Bot " (pack token),
          discordOnLog = print,
          discordOnEvent = eventHandler auctionVar,
          discordOnEnd = putStrLn "Ending",
          discordOnStart = lift $ putStrLn "Starting"
        }
  TIO.appendFile "log.txt" (append userFacingError "\n\n")

eventHandler :: MVar Auctions -> Event -> DiscordHandler ()
eventHandler mvar event = case event of
  MessageCreate m -> if not (fromBot m) then runCommands mvar m commandMap else pure ()
  GuildMemberAdd gId gM -> addRoleToUser gId gM
  InteractionCreate i -> do 
    lift $ pPrint i
    void . restCall $ R.CreateInteractionResponse (interactionId i) (interactionToken i) (InteractionResponseUpdateMessage (InteractionResponseMessage Nothing (Just "Edited") Nothing Nothing Nothing Nothing Nothing))
  _ -> pure ()

runCommands :: MVar Auctions -> Message -> M.Map Text Command -> DiscordHandler ()
runCommands mvar m map = if isJust com then runCommand (fromJust com) mvar m map else pure ()
  where
    commandName = parse parseCommand "Parse Command name" ((toLower . messageContent) m)
    com = if isLeft commandName then Nothing else M.lookup (fromRight "" commandName) map

runCommand :: Command -> MVar Auctions -> Message -> M.Map Text Command -> DiscordHandler ()
runCommand (Com _ (AuctionCommand f)) mvar m _ = f mvar m
runCommand (Com _ (HelpCommand f)) _ m map = f m map
runCommand (Com _ (TextCommand f)) _ m _ = f m
runCommand (Com _ NoOp) _ _ _ = pure ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
