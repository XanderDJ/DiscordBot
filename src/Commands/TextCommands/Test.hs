module Commands.TextCommands.Test where

import Commands.ActionRow (searchActionRow)
import Commands.Types
import Commands.Utility
import Discord
import Discord.Requests
import qualified Discord.Requests as R
import Discord.Types
import Commands.CursorManager (CursorManager, getNewKey)
import Control.Concurrent
import Control.Monad.Trans
import qualified Data.Text as T

testCom :: Command
testCom = Com "ltest - test the feature that I'm currently working on" (CursorCommand testCommand)

testCommand :: MVar CursorManager -> Message -> DiscordHandler ()
testCommand cursorManagerVar m = do
  cursorManager <- lift $ takeMVar cursorManagerVar
  let (key, cm') = getNewKey cursorManager
  lift $ putMVar cursorManagerVar cm'
  sendMessage $ R.CreateMessage (messageChannelId m) "Test"
  sendMessage $ R.CreateMessageDetailed (messageChannelId m) (actionRowMessage key)

actionRowMessage :: T.Text -> R.MessageDetailedOpts
actionRowMessage key =
  def
    { messageDetailedContent = "This is a test",
      messageDetailedComponents = Just [componentActionRow key]
    }

componentActionRow :: T.Text -> ComponentActionRow
componentActionRow t = searchActionRow t False