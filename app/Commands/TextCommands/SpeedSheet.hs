module Commands.TextCommands.SpeedSheet (speedSheetCommand) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Codec.Xlsx ( Worksheet, atSheet, fromXlsx )
import Commands.Types ( Command(..), CommandFunction(TextCommand) )
import Commands.Utility ( pingUserText, sendMessage, invalidMons)
import Control.Concurrent.Async ( mapConcurrently )
import Control.Monad.Trans ( MonadTrans(lift) )
import Data.ByteString.Char8 (intercalate)
import Data.Either ( lefts, rights )
import qualified Data.List as L
import qualified Data.Text as T
import Discord ( DiscordHandler )
import qualified Discord.Requests as R
import Discord.Types ( Message(messageText, messageChannel) )
import Excel
    ( emptySheet, emptyXlsx, insertTable, ExcelTable, Size(width) )
import Commands.Parsers ( parseNOrP, NOrP(..) )
import Pokemon.Excel ( speedTable )
import Pokemon.Functions ( sortOnSpeed )
import Pokemon.PokeApi ( getPokemonNoMoves )
import Pokemon.Types ( Pokemon )
import Text.Parsec ( parse )
import Control.Lens ( (&), (?~) )
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as LS



speedSheetCommand :: Command
speedSheetCommand = Com "lss - makes an excel sheet of speed tiers with the teams specified in the message" (TextCommand makeSpeedSheet)

makeSpeedSheet :: Message -> DiscordHandler ()
makeSpeedSheet m = do
  let text = T.lines (messageText m)
      parsed = map (parse parseNOrP "Parsing team names or pokemon names from list") text
      correctlyParsed = rights parsed
  if null correctlyParsed then spCommandHelp m else makeSpeedSheet' m correctlyParsed

makeSpeedSheet' :: Message -> [NOrP] -> DiscordHandler ()
makeSpeedSheet' m norps = do
  let teams = nOrPsToTeams norps
  if null teams then spCommandHelp m else makeSpeedSheet'' m teams

makeSpeedSheet'' :: Message -> [Team] -> DiscordHandler ()
makeSpeedSheet'' m teams = do
  teams' <- lift $ mapConcurrently mkTeamExcel teams
  let lfts = concat $ getLefts teams'
  if null lfts then makeSS m teams' else invalidMons m lfts

makeSS :: Message -> [TeamExcel] -> DiscordHandler ()
makeSS m teams = do
  let fileName = tsToFileName teams ""
      speedtables = map (speedTable . sortOnSpeed . getMons) teams
      sheet = insertTables speedtables (1, 1) emptySheet
      xl = emptyXlsx & atSheet "SpeedTiers" ?~ sheet
  time <- lift getPOSIXTime
  sendMessage $ R.CreateMessageUploadFile (messageChannel m) fileName (L.toStrict (fromXlsx time xl))

insertTables :: [ExcelTable] -> (Int, Int) -> Worksheet -> Worksheet
insertTables [] _ sheet = sheet
insertTables (t : ts) (x, y) sheet = insertTables ts (x, y + width t + 1) (insertTable (x, y) t sheet)

getMons :: TeamExcel -> [Pokemon]
getMons (TeamExcel _ mons) = rights mons

getLefts :: [TeamExcel] -> [[String]]
getLefts [] = []
getLefts (TeamExcel name mons : ts) = lefts mons : getLefts ts

mkTeamExcel :: Team -> IO TeamExcel
mkTeamExcel (Team name mons) = do
  let mons' = map T.unpack mons
  mons'' <- mapConcurrently getPokemonNoMoves mons'
  return $ TeamExcel name mons''

data Team = Team T.Text [T.Text] deriving (Show)

data TeamExcel = TeamExcel T.Text [Either String Pokemon] deriving (Show)

nOrPsToTeams :: [NOrP] -> [Team]
nOrPsToTeams = go []
  where
    go :: [Team] -> [NOrP] -> [Team]
    go [] [] = []
    go [] ((PN _) : norps) = []
    go ts [] = ts
    go ts ((N name) : norps) = go (Team name [] : ts) norps
    go ts ((PN name) : norps) =
      let t : ts' = ts
          (Team tname mons) = t
          t' = Team tname (name : mons)
       in go (t' : ts') norps

spCommandHelp :: Message -> DiscordHandler ()
spCommandHelp m = sendMessage $ R.CreateMessage (messageChannel m) (T.append (pingUserText m) helpText)

helpText :: T.Text
helpText = ", USAGE \n\nlss <TEAMNAME>:\n<PokemonName>\n\nYou can add as many pokemons as you want per team.\n A new team starts when you add a new name with a :\n\n You can give as many teams as you like, this should normally be only one or two.\nEXAMPLE:\n\nwulf:\ncharizard\nnidoqueen\naegislash blade\npersian alola\nrapidash galar\n\njeroni:\nmeowth\npersian\ncomfey"

tsToFileName :: [TeamExcel] -> T.Text -> T.Text
tsToFileName [] fn = fn
tsToFileName [TeamExcel name _] fn = tsToFileName [] (T.append fn (T.append name ".xlsx"))
tsToFileName (TeamExcel name _ : ts) fn = tsToFileName ts (T.append fn (T.append name "vs"))