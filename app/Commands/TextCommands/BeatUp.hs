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
import qualified PokemonDB.Queries as Q
import Pokemon.DBConversion
import Pokemon.Types
import Text.Parsec
import Database.PostgreSQL.Simple (Connection, close)
import PokemonDB.Connection (getDbConnEnv)

beatUpCom = Com "l(bu|beatup) mon with spaces, mon-without-spaces, mon2" (TextCommand beatUpCommand)

beatUpCommand :: Message -> DiscordHandler ()
beatUpCommand m = do
  let mons = parse parseBeatUp "Parsing beat up command" (messageText m)
  ifElse (isLeft mons || (length (extractRight mons) == 1 && (T.null . head . extractRight) mons)) (beatUpUsage m) (beatUpCommand' m (extractRight mons))


beatUpCommand' :: Message -> [Text] -> DiscordHandler ()
beatUpCommand' m ts = do
  con <- lift $ getDbConnEnv
  ifElse (isNothing con) (noConnection m) (beatUpCommand'' (fromJust con) m ts)

beatUpCommand'' :: Connection -> Message -> [Text] -> DiscordHandler ()
beatUpCommand'' con m ts = do
  mons <- lift $ mapConcurrently (Q.getCompletePokemon con) ts
  lift $ close con
  let lfts = lefts mons
  ifElse (Prelude.null lfts) (beatUpCommand''' m (rights (map (fmap toPokemon) mons))) (invalidMons m lfts)

beatUpCommand''' :: Message -> [Pokemon] -> DiscordHandler ()
beatUpCommand''' m mons = do
  let monsAndAtkStats = map (\mon -> (pName mon, (getValue . getBaseStat "atk") mon)) mons
      bpPerMon = map (second (\a -> 5 + div a 10)) monsAndAtkStats
      totalBp = (sum . map snd) bpPerMon
  sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ bpMessage totalBp bpPerMon))

bpMessage :: Int -> [(Name, Int)] -> String
bpMessage totalBp monsAndBP = ", total bp of beatup with your team: " ++ show totalBp ++ "\n" ++ intercalate "\n" (map showMonsBp monsAndBP)

showMonsBp :: Show a => (T.Text, a) -> [Char]
showMonsBp (mon, bp) = T.unpack mon ++ ": " ++ show bp

beatUpUsage :: Message -> DiscordHandler ()
beatUpUsage m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", usage: l(bu|beatup) mon1, mon 2, mon-3, ...")
