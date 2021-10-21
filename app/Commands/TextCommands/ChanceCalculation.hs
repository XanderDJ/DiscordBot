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

ctCommand :: Message -> DiscordHandler ()
ctCommand m = do
    let parsedMessage = parse parseCT "Parsing for calculate Tries command" (messageText m)
    ifElse (isLeft parsedMessage) (calcTriesUsage m) (ctCommand' m (extractRight parsedMessage))

ctCommand' :: Message -> (Double, Double) -> DiscordHandler ()
ctCommand' m c@(ps, dsr) = do
    ifElse (ps < 0 || dsr < 0 || ps > 1 || dsr > 1) (calcTriesNegative m) (ctCommand'' m c)

ctCommand'' :: Message -> (Double, Double) -> DiscordHandler ()
ctCommand'' m (ps, dsr) = do
    let tries = calculateTries ps dsr
    sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ ", the number of tries needed to achieve your desired success rate is " ++ show tries ++ " tries!" ))

calcTriesUsage :: Message -> DiscordHandler ()
calcTriesUsage m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", l(ct|calctries) (chance of success, 0.5|1/2) (desired successrate, 0.5|1/2)")

calcTriesNegative m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", success chance and desired success rate need to be positive and smaller than 1")
