module Commands.Help (helpCommand) where

import Commands.Types
import Commands.Utility
import qualified Data.Map as M
import Data.Text
import Discord
import Discord.Types
import qualified Discord.Requests as R

help :: Message -> M.Map Text Command -> DiscordHandler ()
help m map = do
    let descriptions = M.foldr appendDescription "" map
    sendMessage $ R.CreateMessage (messageChannel m) descriptions


appendDescription :: Command -> Text -> Text
appendDescription (Com usage _) s = append s (append usage "\n") 

helpCommand :: Command
helpCommand = Com "lhelp" (HelpCommand help)