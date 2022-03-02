{-# LANGUAGE LambdaCase #-}

module Main where

import BotState
import CommandMap
import Commands.ActionRow (searchActionRow)
import Commands.Auction (Auctions)
import Commands.CursorManager
import Commands.Manage.Role
import Commands.Parsers
import Commands.Types
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Default (Default (def))
import Data.Either
import Data.Functor
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
  ( DiscordHandler,
    RunDiscordOpts (discordOnEnd, discordOnEvent, discordOnLog, discordOnStart, discordToken),
    restCall,
    runDiscord,
  )
import qualified Discord
import Discord.Interactions
import qualified Discord.Requests as R
import Discord.Types
  ( ComponentActionRow (ComponentActionRowButton, ComponentActionRowSelectMenu),
    ComponentButton (componentButtonCustomId),
    Event (GuildMemberAdd, GuildMemberUpdate, InteractionCreate, MessageCreate),
    Message (messageAuthor, messageComponents, messageContent, messageId),
    User (userIsBot),
  )
import Interactions.CursorInteraction (handleCursorInteraction)
import System.Environment (getArgs)
import System.Random (getStdGen)
import Text.Parsec
import Text.Pretty.Simple (pPrint)
import Utility

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
  MessageCreate m -> if not (fromBot m) then runCommands botState m commandMap else linkMessage (cursorManager botState) m
  GuildMemberAdd gId gM -> addRoleToUser gId gM
  InteractionCreate i -> do
    case i of
      i@(InteractionComponent sn sn' idc m_sn ma mou txt n mes txt' m_txt) -> case idc of
        InteractionDataComponentButton cId -> do
          cm <- lift $ readMVar (cursorManager botState)
          let key = parseToken cId
          when (hasKey cm key && isCursorUser cm (interactionUser i) (fromJust key)) (handleCursorInteraction cId (cursorManager botState) i)
        InteractionDataComponentSelectMenu txt2 txts -> pure ()
      InteractionPing sn sn' txt n -> void . restCall $ R.CreateInteractionResponse sn txt InteractionResponsePong
      InteractionApplicationCommand
        sn
        sn'
        idac
        m_sn
        ma
        mou
        txt
        n
        txt'
        m_txt ->
          pure ()
      InteractionApplicationCommandAutocomplete
        sn
        sn'
        idac
        m_sn
        ma
        mou
        txt
        n
        txt'
        m_txt ->
          pure ()
      InteractionModalSubmit sn sn' idm m_sn ma mou txt n txt' m_txt -> pure ()
  _ -> pure ()

runCommands :: BotState -> Message -> M.Map Text Command -> DiscordHandler ()
runCommands botState m map = if isJust com then runCommand (fromJust com) botState m map else pure ()
  where
    commandName = parse parseCommand "Parse Command name" ((T.toLower . messageContent) m)
    com = if isLeft commandName then Nothing else M.lookup (fromRight "" commandName) map

runCommand :: Command -> BotState -> Message -> M.Map Text Command -> DiscordHandler ()
runCommand (Com _ (AuctionCommand f)) botState m _ = f (auctionState botState) m
runCommand (Com _ (HelpCommand f)) _ m map = f map m
runCommand (Com _ (TextCommand f)) _ m _ = f m
runCommand (Com _ (CursorCommand f)) botState m _ = f (cursorManager botState) m
runCommand (Com _ NoOp) _ _ _ = pure ()

linkMessage :: MVar CursorManager -> Message -> DiscordHandler ()
linkMessage cursorManagerVar m = do
  cursorManager <- lift $ takeMVar cursorManagerVar
  let key = getKey m
      cm' = linkMessageId cursorManager key (messageId m)
  lift $ putMVar cursorManagerVar cm'
  pure ()

getKey :: Message -> Maybe Text
getKey m =
  ( messageComponents m
      >>= maybeHead
      >>= ( \case
              ComponentActionRowButton cbs -> maybeHead cbs
              ComponentActionRowSelectMenu csm -> Nothing
          )
  )
    >>= parseToken . componentButtonCustomId
  where
    maybeHead [] = Nothing
    maybeHead (x : xs) = Just x

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
