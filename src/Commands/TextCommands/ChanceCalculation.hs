module Commands.TextCommands.ChanceCalculation where

import ChanceCalculator
import Commands.Types
import Commands.Utility
import Control.Monad
import Control.Monad.Trans
import Data.Either
import Data.Maybe
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Commands.Parsers
import Text.Parsec
import Data.Text

ctCom :: Command
ctCom = Com "l(calctries|ct) (chance of success, 0.9 or 9/10) (desired success rate, 0.95 or 95/100)" (TextCommand ctCommand)

ccCom :: Command
ccCom = Com "l(calcChance|cc) (chance of occurring, 0.9|9/10) (times tried)" (TextCommand ccCommand)

ccCommand :: Message -> DiscordHandler ()
ccCommand m = do
    let parsedMessage = parse parseCC "Parsing for calculate Chance command" (messageContent m)
    ifElse (isLeft parsedMessage) (calcTriesUsage m) (ccCommand' m (extractRight parsedMessage))


ctCommand :: Message -> DiscordHandler ()
ctCommand m = do
    let parsedMessage = parse parseCT "Parsing for calculate Tries command" (messageContent m)
    ifElse (isLeft parsedMessage) (calcChanceUsage m) (ctCommand' m (extractRight parsedMessage))

ccCommand' :: Message -> (Double, Int) -> DiscordHandler ()
ccCommand' m c@(ps, tries) = do
    ifElse (ps < 0 || tries < 0 || ps > 1) (calcChanceNegative m) (ccCommand'' m c)

ccCommand'' :: Message -> (Double, Int) -> DiscordHandler ()
ccCommand'' m (ps, tries) = do
    let chance = chanceFor ps tries
    sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) (pack $ ", the chance of the event happening at least once after " ++ show tries ++ " tries is: " ++ show chance ++ "!"))

ctCommand' :: Message -> (Double, Double) -> DiscordHandler ()
ctCommand' m c@(ps, dsr) = do
    ifElse (ps < 0 || dsr < 0 || ps > 1 || dsr > 1) (calcTriesNegative m) (ctCommand'' m c)

ctCommand'' :: Message -> (Double, Double) -> DiscordHandler ()
ctCommand'' m (ps, dsr) = do
    let tries = calculateTries ps dsr
    sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) (pack $ ", the number of tries needed to achieve your desired success rate is " ++ show tries ++ " tries!" ))

calcTriesUsage :: Message -> DiscordHandler ()
calcTriesUsage m = sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", l(ct|calctries) (chance of success, 0.5|1/2) (desired successrate, 0.5|1/2)")

calcChanceUsage :: Message -> DiscordHandler ()
calcChanceUsage m = sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", l(cc|calcchance) (chance of success, 0.5|1/2) (times tried, 13)")

calcTriesNegative :: Message -> DiscordHandler ()
calcTriesNegative m = sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", success chance and desired success rate need to be positive and smaller than 1")

calcChanceNegative :: Message -> DiscordHandler ()
calcChanceNegative m = sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", success chance needs to be positive and smaller than 1, times tried needs to be positive")

