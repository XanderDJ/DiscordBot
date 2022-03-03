module Commands.Debug.ShowState where
import Commands.Types
import BotState
import Discord.Types
import Discord
import Control.Monad.Trans
import Text.Pretty.Simple
import Control.Concurrent (readMVar)

showStateCom = Com "Shows state in console" (StateCommand showStateCommand)

showStateCommand :: BotState -> Message -> DiscordHandler ()
showStateCommand (BotState auctionsVar cursorManagerVar) m = do
    cursorManager <- lift $ readMVar cursorManagerVar
    lift $ pPrint cursorManager