module Commands.TextCommands.BeatUp (beatUpCom) where

import Commands.Parsers
import Commands.Types
import Commands.Utility
import Control.Arrow
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Trans
import Data.Either
import Data.List (intercalate)
import Data.Maybe
import Data.Text (Text, append, pack, unpack)
import qualified Data.Text as T
import Discord
import qualified Discord.Requests as R
import Discord.Types 
import Pokemon.Functions
import Pokemon.PokeApi
import Pokemon.Types
import Text.Parsec

beatUpCom = Com "l(bu|beatup) mon with spaces, mon-without-spaces, mon2" (TextCommand beatUpCommand)

beatUpCommand :: Message -> DiscordHandler ()
beatUpCommand m = do
  let mons = parse parseBeatUp "Parsing beat up command" (messageText m)
  ifElse (isLeft mons || (length (extractRight mons) == 1 && (T.null . head . extractRight) mons)) (beatUpUsage m) (beatUpCommand' m (extractRight mons))

beatUpCommand' :: Message -> [Text] -> DiscordHandler ()
beatUpCommand' m ts = do
  mons <- lift $ mapConcurrently (getPokemonNoMoves . unpack) ts
  let lfts = lefts mons
  ifElse (Prelude.null lfts) (beatUpCommand'' m (rights mons)) (invalidMons m lfts)

beatUpCommand'' :: Message -> [Pokemon] -> DiscordHandler ()
beatUpCommand'' m mons = do
  let monsAndAtkStats = map (\mon -> (pName mon, (getValue . getBaseStat "atk") mon)) mons
      bpPerMon = map (second (\a -> 5 + div a 10)) monsAndAtkStats
      totalBp = (sum . map snd) bpPerMon
  sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ bpMessage totalBp bpPerMon))

bpMessage :: Int -> [(Name, Int)] -> String
bpMessage totalBp monsAndBP = ", total bp of beatup with your team: " ++ show totalBp ++ "\n" ++ intercalate "\n" (map showMonsBp monsAndBP)

showMonsBp :: Show a => ([Char], a) -> [Char]
showMonsBp (mon, bp) = mon ++ ": " ++ show bp

beatUpUsage :: Message -> DiscordHandler ()
beatUpUsage m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", usage: l(bu|beatup) mon1, mon 2, mon-3, ...")
