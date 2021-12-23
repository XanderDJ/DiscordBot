module Commands.TextCommands.ValueConversion where

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Commands.Types (Command(Com), CommandFunction (TextCommand))
import Commands.Parsers (parseOptions, parseIntCommand)
import Text.Parsec
import Commands.Utility (ifElse, extractRight, reportError, getOption, sendMessage, pingUserText)
import Data.Either
import Data.Maybe
import Text.Read (readMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List as L
import Pokemon.DamageCalc (printPercentage)


valueConversionCom = Com "lvc <value> [--(ob,oldbudget) budget| 80000] [--(opa|oldamount|oldplayeramount) amount of players | 5] [--(b,budget) newBudget | 95000] [--(npa|newplayeramount|newamount) new amount of players | 8]" (TextCommand valueConversionCommand)

valueConversionCommand :: Message -> DiscordHandler ()
valueConversionCommand m = do
    let (msg, opts) = parseOptions (messageText m)
        value = parse (parseIntCommand "lvc") "" msg
    ifElse (isRight value || isNothing (extractRight value)) (usageValueConversion m) (validData ((fromJust . extractRight) value) opts m)

usageValueConversion = reportError "lvc <value> [--(ob,oldbudget) budget| 80000] [--(opa|oldamount|oldplayeramount) amount of players | 5] [--(b,budget) newBudget | 95000] [--(npa|newplayeramount|newamount) new amount of players | 8]"

validData :: Int -> M.Map T.Text T.Text -> Message -> DiscordHandler ()
validData oldVal opts m = do
    let oldBudget = fromMaybe 80000 (getOption ["oldbudget", "ob"] opts >>= (readMaybe . T.unpack))
        oldAmount = fromMaybe 5 (getOption ["oldplayeramount", "oldamount", "opa"] opts >>= (readMaybe . T.unpack))
        newBudget = fromMaybe 95000 (getOption ["b", "budget"] opts >>= (readMaybe . T.unpack))
        newAmount = fromMaybe 8 (getOption ["npa", "newplayeramount", "newamount"] opts >>= (readMaybe . T.unpack))
        ratio = (newBudget / newAmount) / (oldBudget / oldAmount) :: Double
        newVal = fromIntegral oldVal * ratio  
    sendMessage $ R.CreateMessage (messageChannel m) (T.append (pingUserText m) (L.foldl T.append "" [", the given value would be equal to ", T.pack (printPercentage 2 newVal), " units"]))