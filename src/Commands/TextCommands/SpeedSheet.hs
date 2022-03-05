module Commands.TextCommands.SpeedSheet (speedSheetCommand) where

import Codec.Xlsx (Worksheet, atSheet, fromXlsx)
import Commands.Parsers (NOrP (..), parseNOrP)
import Commands.Types (Command (..), CommandFunction (TextCommand))
import Commands.Utility (ifElse, invalidMons, noConnection, pingUserText, pokemonDb, sendMessage)
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens ((&), (?~))
import Control.Monad.Trans (MonadTrans (lift))
import qualified Data.ByteString as LS
import Data.ByteString.Char8 (intercalate)
import qualified Data.ByteString.Lazy as L hiding (concat)
import Data.Either (lefts, rights)
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection, close)
import Discord (DiscordHandler)
import qualified Discord.Requests as R
import Discord.Types
import Excel
import Pokemon.DBConversion (toPokemon)
import Pokemon.Excel (pokemonMoveMap, speedTable)
import Pokemon.Functions (sortOnSpeed)
import Pokemon.Types
import PokemonDB.Connection (getDbConnEnv)
import qualified PokemonDB.Queries as Q
import Text.Parsec (parse)
import Discord

speedSheetCommand :: Command
speedSheetCommand = Com "lss - makes an excel sheet of speed tiers with the teams specified in the message" (TextCommand makeSpeedSheet)

makeSpeedSheet :: Message -> DiscordHandler ()
makeSpeedSheet m = do
  let text = T.lines (messageContent m)
      parsed = map (parse parseNOrP "Parsing team names or pokemon names from list") text
      correctlyParsed = rights parsed
  if null correctlyParsed then spCommandHelp m else makeSpeedSheet' m correctlyParsed

makeSpeedSheet' :: Message -> [NOrP] -> DiscordHandler ()
makeSpeedSheet' m norps = do
  let teams = nOrPsToTeams norps
  if null teams then spCommandHelp m else pokemonDb (makeSpeedSheet'' teams) m

makeSpeedSheet'' :: [Team] -> Connection -> Message -> DiscordHandler ()
makeSpeedSheet'' teams con m = do
  teams' <- lift $ mapConcurrently (mkTeamExcel con) teams
  lift $ close con
  let lfts = concat $ getLefts teams'
  if null lfts then makeSS m teams' else invalidMons m lfts

makeSS :: Message -> [TeamExcel] -> DiscordHandler ()
makeSS m teams = do
  let teams' = reverse teams
      fileName = tsToFileName teams' ""
      speedtables = map (speedTable . sortOnSpeed . getMons) teams'
      sheet = insertTables speedtables (1, 1) emptySheet
      teams'' = tail teams'
      moveMaps = map (map (\pokemon -> (pName pokemon, pokemonMoveMap HORIZONTAL pokemon)) . L.nub . getMons) teams''
      xl' = emptyXlsx & atSheet "SpeedTiers" ?~ sheet
      xl = insertMoveMaps xl' (L.concat moveMaps)
  time <- lift getPOSIXTime
  sendMessage $ R.CreateMessageDetailed (messageChannelId m) def {R.messageDetailedFile = Just (fileName, L.toStrict (fromXlsx time xl))}

insertTables :: [ExcelTable] -> (Int, Int) -> Worksheet -> Worksheet
insertTables [] _ sheet = sheet
insertTables (t : ts) (x, y) sheet = insertTables ts (x, y + width t + 1) (insertTable (x, y) t sheet)

getMons :: TeamExcel -> [Pokemon]
getMons (TeamExcel _ mons) = rights mons

getLefts :: [TeamExcel] -> [[T.Text]]
getLefts [] = []
getLefts (TeamExcel name mons : ts) = lefts mons : getLefts ts

mkTeamExcel :: Connection -> Team -> IO TeamExcel
mkTeamExcel con (Team name mons) = do
  mons' <- mapConcurrently (Q.getCompletePokemon con) mons
  return $ TeamExcel name (map (fmap toPokemon) mons')

data Team = Team T.Text [T.Text] deriving (Show)

data TeamExcel = TeamExcel T.Text [Either T.Text Pokemon] deriving (Show)

nOrPsToTeams :: [NOrP] -> [Team]
nOrPsToTeams = go []
  where
    go :: [Team] -> [NOrP] -> [Team]
    go [] [] = []
    go [] ((PN _) : norps) = []
    go ts [] = ts
    go ts ((N name) : norps) = go (Team name [] : ts) norps
    go ts ((PN "") : norps) = go ts norps
    go ts ((PN name) : norps) =
      let t : ts' = ts
          (Team tname mons) = t
          t' = Team tname (name : mons)
       in go (t' : ts') norps

spCommandHelp :: Message -> DiscordHandler ()
spCommandHelp m = sendMessage $ R.CreateMessage (messageChannelId m) (T.append (pingUserText m) helpText)

helpText :: T.Text
helpText = ", USAGE \n\nlss <TEAMNAME>:\n<PokemonName>\n\nYou can add as many pokemons as you want per team.\n A new team starts when you add a new name with a :\n\n You can give as many teams as you like, this should normally be only one or two.\nEXAMPLE:\n\nplayer1:\ncharizard\nblastoise\nvenusaur mega\n\n\nplayer2:\npersian-   alola\nrapidash galar\ncharizard mega y"

tsToFileName :: [TeamExcel] -> T.Text -> T.Text
tsToFileName [] fn = fn
tsToFileName [TeamExcel name _] fn = tsToFileName [] (T.append fn (T.append name ".xlsx"))
tsToFileName (TeamExcel name _ : ts) fn = tsToFileName ts (T.append fn (T.append name "vs"))
