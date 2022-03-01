module Commands.Help (helpCommand) where

import Commands.Types
import Commands.Utility
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text
import Discord
import Discord.Types
import qualified Discord.Requests as R

help :: M.Map Text Command -> Message -> DiscordHandler ()
help map m = do
    let 
        commands = L.nub . M.elems $ map
        descriptions = L.foldr appendDescription "" commands
    sendMessage $ R.CreateMessage (messageChannelId m) descriptions


appendDescription :: Command -> Text -> Text
appendDescription (Com usage _) s = append s (append usage "\n") 

helpCommand :: Command
helpCommand = Com "lhelp" (HelpCommand help)