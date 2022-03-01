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
import BotState
import Commands.CursorManager

main = bot

bot :: IO ()
bot = do
  [token] <- getArgs
  auctionVar <- newEmptyMVar
  putMVar auctionVar []
  cursorManagerVar <- newEmptyMVar
  stdGen <- getStdGen
  putMVar cursorManagerVar (CursorManager M.empty stdGen)
  userFacingError <-
    runDiscord $
      def
        { discordToken = append "Bot " (pack token),
          discordOnLog = print,
          discordOnEvent = eventHandler (BotState auctionVar cursorManagerVar),
          discordOnEnd = putStrLn "Ending",
          discordOnStart = lift $ putStrLn "Starting"
        }
  TIO.appendFile "log.txt" (append userFacingError "\n\n")

eventHandler :: BotState -> Event -> DiscordHandler ()
eventHandler botState event = case event of
  MessageCreate m -> if not (fromBot m) then runCommands botState m commandMap else pure ()
  GuildMemberAdd gId gM -> addRoleToUser gId gM
  InteractionCreate i -> do 
    lift $ pPrint i
    void . restCall $ R.CreateInteractionResponse (interactionId i) (interactionToken i) (InteractionResponseUpdateMessage (InteractionResponseMessage Nothing (Just "Edited") Nothing Nothing Nothing Nothing Nothing))
  _ -> pure ()

runCommands :: BotState -> Message -> M.Map Text Command -> DiscordHandler ()
runCommands botState m map = if isJust com then runCommand (fromJust com) botState m map else pure ()
  where
    commandName = parse parseCommand "Parse Command name" ((toLower . messageContent) m)
    com = if isLeft commandName then Nothing else M.lookup (fromRight "" commandName) map

runCommand :: Command -> BotState -> Message -> M.Map Text Command -> DiscordHandler ()
runCommand (Com _ (AuctionCommand f)) botState m _ = f (auctionState botState) m
runCommand (Com _ (HelpCommand f)) _ m map = f map m
runCommand (Com _ (TextCommand f)) _ m _ = f m
runCommand (Com _ (CursorCommand f)) botState m _ = f (cursorManager botState) m 
runCommand (Com _ NoOp) _ _ _ = pure ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
