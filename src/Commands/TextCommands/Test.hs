module Commands.TextCommands.Test where

import Commands.Types
import Commands.Utility
import Discord
import Discord.Requests
import qualified Discord.Requests as R
import Discord.Types

testCom :: Command
testCom = Com "ltest - test the feature that I'm currently working on" (TextCommand testCommand)

testCommand :: Message -> DiscordHandler ()
testCommand m = do
  sendMessage $ R.CreateMessageDetailed (messageChannelId m) actionRowMessage

actionRowMessage :: R.MessageDetailedOpts
actionRowMessage =
  def
    { messageDetailedContent = "This is a test",
      messageDetailedComponents = Just [componentActionRow]
    }

componentActionRow :: ComponentActionRow
componentActionRow =
  ComponentActionRowButton
    [ ComponentButton "test" False ButtonStylePrimary (Just "Test") Nothing, ComponentButton "test" False ButtonStylePrimary (Just "Forward") Nothing 
    ]