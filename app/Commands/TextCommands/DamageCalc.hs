{-# LANGUAGE RecordWildCards #-}

module Commands.TextCommands.DamageCalc (damageCalcCom) where

import Commands.Parsers
import Commands.Types
import Commands.Utility
import Control.Monad (when)
import Control.Monad.Trans
import Data.Either
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe
import Data.StatMultiplier
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Pokemon.DBConversion
import Pokemon.DamageCalc.DamageCalc (runCalc)
import Pokemon.DamageCalc.Types
import Pokemon.Nature (adamant, bold, calm, careful, gentle, getNature, impish, jolly, modest, serious, timid)
import Pokemon.Types
import PokemonDB.Queries
import PokemonDB.Types
import Text.Read (readMaybe)

damageCalcCom :: Command
damageCalcCom = Com "ldc mon1 --options, move --environmentoptions, mon2 --options" (TextCommand dcCommand)

dcCommand :: Message -> DiscordHandler ()
dcCommand msg = do
  let parsedMessage = parseDC (messageText msg)
  ifElse (isLeft parsedMessage) (dcUsage msg) (pokemonDb (validMessage (extractRight parsedMessage)) msg)

validMessage :: (T.Text, T.Text, T.Text) -> Connection -> Message -> DiscordHandler ()
validMessage (mt1, mt, mt2) con m = do
  let (mon1', m1Opts) = parseOptions mt1
      (move', moveOpts) = parseOptions mt
      (mon2', m2Opts) = parseOptions mt2
      mon1 = cleanMon mon1'
      mon2 = cleanMon mon2'
      move = toId move'
      item1 = toId <$> getOption ["i", "item", "it"] m1Opts
      item2 = toId <$> getOption ["i", "item", "it"] m2Opts
  m1 <- lift $ getCompletePokemon con mon1
  m2 <- lift $ getCompletePokemon con mon2
  mv <- lift $ getMove con move
  i1 <- if isJust item1 then do Just <$> lift (getItem con (fromJust item1)) else return Nothing
  i2 <- if isJust item2 then do Just <$> lift (getItem con (fromJust item2)) else return Nothing
  let pokemonErrMsg = T.intercalate ", " (pokemonErr [m1, m2])
      moveErrMsg = if length mv /= 1 then move else ""
      itemErrMsg = T.intercalate ", " (itemErr [(item1, i1), (item2, i2)])
      errorMsg = errorMessage pokemonErrMsg moveErrMsg itemErrMsg
  lift $ close con
  ifElse (T.null errorMsg) (validDbData (extractRight m1, m1Opts) (extractRight m2, m2Opts) (head mv, moveOpts) (head <$> i1) (head <$> i2) m) (reportError errorMsg m)
  where
    pokemonErr [] = []
    pokemonErr ((Left id) : ms) = id : pokemonErr ms
    pokemonErr ((Right _) : ms) = pokemonErr ms
    itemErr [] = []
    itemErr ((Nothing, _) : is) = itemErr is
    itemErr ((Just iId, Just i) : is) = if length i /= 1 then iId : itemErr is else itemErr is
    itemErr _ = error "Unreachable situation"
    cleanMon = toId . T.strip . T.replace "ldc" "" . T.replace "ldamagecalc" ""

validDbData :: (DBCompletePokemon, M.Map T.Text T.Text) -> (DBCompletePokemon, M.Map T.Text T.Text) -> (DBMove, M.Map T.Text T.Text) -> Maybe DBItem -> Maybe DBItem -> Message -> DiscordHandler ()
validDbData (mon1, m1Opts) (mon2, m2Opts) (move, moveOpts) i1 i2 m = do
  let env = parseEnv moveOpts
      effectiveMove = parseMove move moveOpts
      parsedMon1 = parseMon (toPokemon mon1) (toItem <$> i1) m1Opts effectiveMove
      parsedMon2 = parseMon (toPokemon mon2) (toItem <$> i2) m2Opts effectiveMove
      damageCalcState = DCS env parsedMon1 parsedMon2 effectiveMove
      calc = runCalc damageCalcState
  if not (hasOption ["debug", "d"] moveOpts)
    then sendMessage $ R.CreateMessage (messageChannel m) (T.pack (show (fst calc)))
    else sendMessage $ R.CreateMessage (messageChannel m) (T.pack (show (snd calc)))
  -- calcMessage = makeCalcMessage (min, max) damageCalcState
  printIO damageCalcState
  printIO (snd calc)

makeCalcMessage :: (Int, Int) -> DCS -> T.Text
makeCalcMessage (min, max) DCS {dcsEnv = Env {..}} = undefined

parseMon :: Pokemon -> Maybe Item -> M.Map T.Text T.Text -> EffectiveMove -> EffectivePokemon
parseMon Pokemon {..} i opts EM {..} =
  EP
    { epName = toId pName,
      epAbility = fromMaybe ((toId . head) abilities) (getOption ["a", "ability"] opts >>= \s -> return (toId s)),
      epTyping = typing,
      epStats = baseStats,
      epLevel = let level = getOption ["l", "level"] opts in fromMaybe 100 (level >>= \l -> readMaybe (T.unpack l)),
      epItem = toIdItem <$> i,
      epNature = fromMaybe (maybe (getDefaultNature emCategory) third set) nature,
      epEvs = maybe (EVS hpev atkev defev spaev spdev speev) first set,
      epIvs = maybe (IVS hpiv atkiv defiv spaiv spdiv speiv) second set,
      epStatus = let status = opts M.!? "status" in status >>= \s -> readMaybe (T.unpack s),
      epNfe = pNfe,
      epWeight = pWeight,
      epRisen = risen,
      epMultiplier = Multipliers atkMult defMult spaMult spdMult speMult,
      epHPPercentage = hpPercentage,
      epStatsLowered = hasOption ["statslowered", "sl", "lashout", "lash"] opts,
      epFlashFire = hasOption ["flashfire", "flash", "ff"] opts
    }
  where
    hpPercentage = fromMaybe 100 (getOption ["percentage", "hp", "hp%"] opts >>= readMaybe . T.unpack >>= toPercentage)
    risen = hasOption ["telekinesis", "tele", "tel", "magnetrise", "magnet", "mr", "levitating", "levitate"] opts
    t' = getOption ["type", "t"] opts >>= \t -> let tt = T.splitOn ";" t in let rs = mapMaybe (readMaybe . T.unpack . toId . T.strip) tt in if null rs then Nothing else Just rs
    typing = fromMaybe pTyping t'
    atkMult = fromMaybe (0 %% 0) ((getOption ["atkm", "atkmultiplier", "attackmultiplier", "attackboost", "atkboost", "atkb"] opts >>= readMaybeInt . T.unpack) <&> fromIntegral)
    defMult = fromMaybe (0 %% 0) ((getOption ["defm", "defmultiplier", "defensemultiplier", "defboost", "defb", "defenseboost"] opts >>= readMaybeInt . T.unpack) <&> fromIntegral)
    spaMult = fromMaybe (0 %% 0) ((getOption ["spam", "spamultiplier", "specialattackmultiplier", "specialattackboost", "spab", "spaboost", "specialattackb"] opts >>= readMaybeInt . T.unpack) <&> fromIntegral)
    spdMult = fromMaybe (0 %% 0) ((getOption ["spdm", "spdmultiplier", "specialdefensemultiplier", "spdb", "spdboost", "specialdefenseboost"] opts >>= readMaybeInt . T.unpack) <&> fromIntegral)
    speMult = fromMaybe (0 %% 0) ((getOption ["spem", "spemultiplier", "speedmultiplier", "speb", "speboost", "speedboost"] opts >>= readMaybeInt . T.unpack) <&> fromIntegral)

    readMaybeInt :: String -> Maybe Int
    readMaybeInt = readMaybe
    hpiv = fromMaybe 31 (getOption ["hpiv"] opts >>= \iv -> readMaybe (T.unpack iv))
    atkiv = fromMaybe 31 (getOption ["attackiv", "atkiv"] opts >>= \iv -> readMaybe (T.unpack iv))
    defiv = fromMaybe 31 (getOption ["defenseiv", "defiv"] opts >>= \iv -> readMaybe (T.unpack iv))
    spaiv = fromMaybe 31 (getOption ["specialattackiv", "spaiv", "spatkiv"] opts >>= \iv -> readMaybe (T.unpack iv))
    spdiv = fromMaybe 31 (getOption ["specialdefenseiv", "spdiv", "spdefiv"] opts >>= \iv -> readMaybe (T.unpack iv))
    speiv = fromMaybe 31 (getOption ["speediv", "speiv"] opts >>= \iv -> readMaybe (T.unpack iv))
    hpev = fromMaybe 0 (getOption ["hpev"] opts >>= \ev -> readMaybe (T.unpack ev))
    atkev = fromMaybe 252 (getOption ["attackev", "atkev"] opts >>= \ev -> readMaybe (T.unpack ev))
    defev = fromMaybe 0 (getOption ["defenseev", "defev"] opts >>= \ev -> readMaybe (T.unpack ev))
    spaev = fromMaybe 252 (getOption ["specialattackev", "spaev", "spatkev"] opts >>= \ev -> readMaybe (T.unpack ev))
    spdev = fromMaybe 0 (getOption ["specialdefenseev", "spdev", "spdefev"] opts >>= \ev -> readMaybe (T.unpack ev))
    speev = fromMaybe 252 (getOption ["speedev", "speev"] opts >>= \ev -> readMaybe (T.unpack ev))
    getDefaultNature PHYSICAL = jolly
    getDefaultNature SPECIAL = timid
    getDefaultNature OTHER = serious
    nature = getOption ["nature", "n"] opts >>= getNature
    set = getOption ["set", "s"] opts >>= \s -> parseSet emCategory (toId s)
    first (a, b, c) = a
    second (a, b, c) = b
    third (a, b, c) = c
    toPercentage x
      | x < 0 = Just 0
      | x > 100 = Just 100
      | otherwise = Just x

toIdItem :: Item -> Item
toIdItem i@Item {..} = i {iName = toId iName}

parseSet :: AttackType -> T.Text -> Maybe (EVs, IVs, Nature)
parseSet at "hyperoffense" = Just $ getCorrectType at (maxAtkEVs, usualIvs, jolly) (maxSpaEVs, specialIvs, timid)
parseSet at "ho" = Just $ getCorrectType at (maxAtkEVs, usualIvs, jolly) (maxSpaEVs, specialIvs, timid)
parseSet at "bulkyoffense" = Just $ getCorrectType at (bulkyAtkEVs, usualIvs, adamant) (bulkySpaEVs, specialIvs, modest)
parseSet at "bo" = Just $ getCorrectType at (bulkyAtkEVs, usualIvs, adamant) (bulkySpaEVs, specialIvs, modest)
parseSet at "bulkysweeper" = Just $ getCorrectType at (bulkyAtkEVs, usualIvs, adamant) (bulkySpaEVs, specialIvs, modest)
parseSet at "fullspdef" = Just $ getCorrectType at (maxSpdefEvs, usualIvs, careful) (maxSpdefEvs, specialIvs, calm)
parseSet at "fulldef" = Just $ getCorrectType at (maxDefEvs, usualIvs, impish) (maxDefEvs, specialIvs, bold)
parseSet at "maxdef" = Just $ getCorrectType at (maxDefEvs, usualIvs, impish) (maxDefEvs, specialIvs, bold)
parseSet at "maxspdef" = Just $ getCorrectType at (maxSpdefEvs, usualIvs, careful) (maxSpdefEvs, specialIvs, calm)
parseSet at "utility" = Just $ getCorrectType at (utilityEVs, usualIvs, jolly) (utilityEVs, specialIvs, timid)
parseSet at "util" = Just $ getCorrectType at (utilityEVs, usualIvs, jolly) (utilityEVs, specialIvs, timid)
parseSet at "trickroom" = Just $ getCorrectType at (maxAtkEVs, trickroomIvs, adamant) (maxSpaEVs, trickroomSIvs, modest)
parseSet at "tr" = Just $ getCorrectType at (maxAtkEVs, trickroomIvs, adamant) (maxSpaEVs, trickroomSIvs, modest)
parseSet _ _ = Nothing

getCorrectType PHYSICAL t _ = t
getCorrectType SPECIAL _ t = t
getCorrectType OTHER t _ = t

maxAtkEVs :: EVs
maxAtkEVs = EVS 0 252 0 0 4 252

bulkyAtkEVs :: EVs
bulkyAtkEVs = EVS 252 252 0 0 4 0

maxSpaEVs :: EVs
maxSpaEVs = EVS 0 0 0 252 4 252

bulkySpaEVs :: EVs
bulkySpaEVs = EVS 252 0 0 252 4 0

utilityEVs :: EVs
utilityEVs = EVS 252 0 0 0 4 252

maxSpdefEvs :: EVs
maxSpdefEvs = EVS 252 0 4 0 252 0

maxDefEvs :: EVs
maxDefEvs = EVS 252 0 252 0 4 0

usualIvs :: IVs
usualIvs = IVS 31 31 31 31 31 31

specialIvs :: IVs
specialIvs = IVS 31 0 31 31 31 31

trickroomIvs :: IVs
trickroomIvs = IVS 31 31 31 31 31 0

trickroomSIvs :: IVs
trickroomSIvs = IVS 31 0 31 31 31 0

parseEnv :: M.Map T.Text T.Text -> Environment
parseEnv m = Env terrain weather s c g mr wr tr ps b electrified tw minimized invulnerable lc hb so pr mpr double
  where
    terrain = getOption ["terrain", "t"] m >>= \t -> parseTerrain (toId t)
    weather = getOption ["weather", "w"] m >>= \w -> parseWeather (toId w)
    s' = getOption ["screens", "screen", "scr"] m >>= \s -> let ss = T.splitOn ";" s in let rs = mapMaybe (parseScreen . toId . T.strip) ss in if null rs then Nothing else Just rs
    s = fromMaybe [] s'
    c = hasOption ["crit", "criticalhit", "c", "critical-hit"] m
    g = hasOption ["gravity", "g", "grav"] m
    mr = hasOption ["magicroom", "magic", "embargo", "mr"] m
    wr = hasOption ["wonderroom", "wonder", "wr"] m
    tr = hasOption ["tr", "trickroom", "trick-room", "troom"] m
    ps = hasOption ["powerspot", "power"] m
    b = hasOption ["battery", "batt"] m
    tw = hasOption ["tailwind", "tail", "tw", "wind"] m
    invulnerable = hasOption ["diving", "dive", "digging", "dig", "invulnerable", "flying", "phantomforce", "phantom", "fly"] m
    minimized = hasOption ["minimized", "minimize", "min"] m
    electrified = hasOption ["electrify", "electrified", "iondeluge", "ion", "ion-deluge"] m
    pr = hasOption ["protect", "prot"] m
    lc = hasOption ["luckychant", "lucky-chant", "lc"] m
    hb = hasOption ["hit", "hitbefore"] m
    so = hasOption ["switchingout", "switching", "switch"] m
    mpr = hasOption ["maxguard", "maxg", "maxprot", "maxprotect"] m
    double = hasOption ["double", "doublebattle"] m

parseMove :: DBMove -> M.Map T.Text T.Text -> EffectiveMove
parseMove move@MoveT {moveBp = bp, moveType = tipe} opts =
  em
    { emBp = fromMaybe bp ((opts M.!? "bp") >>= readMaybe . T.unpack),
      emType = fromMaybe ((read . T.unpack) tipe) (getOption ["type"] opts >>= (readMaybe . T.unpack . toId . T.strip)),
      emTimesUsed = timesUsed,
      emHits = hits,
      emStockPile = getOption ["stockpile", "stock", "sp"] opts >>= readMaybe . T.unpack
    }
  where
    em = toEffectiveMove move
    timesUsed = fromMaybe 0 (getOption ["timesused", "tu", "times"] opts >>= readMaybe . T.unpack)
    hits = fromMaybe (getDefaultHits (emName em)) (getOption ["hits", "h"] opts >>= readMaybe . T.unpack)

getDefaultHits :: T.Text -> Int
getDefaultHits "dualwingbeat" = 2
getDefaultHits _ = 1

errorMessage :: T.Text -> T.Text -> T.Text -> T.Text
errorMessage "" "" "" = ""
errorMessage pErr "" "" = T.append "Couldn't find the following mons: " pErr
errorMessage "" mErr "" = T.append "Not a valid move: " mErr
errorMessage "" "" iErr = T.append "Couldn't find the following items: " iErr
errorMessage pErr mErr "" = T.intercalate "\n" [errorMessage pErr "" "", errorMessage "" mErr ""]
errorMessage pErr "" iErr = T.intercalate "\n" [errorMessage pErr "" "", errorMessage "" "" iErr]
errorMessage "" mErr iErr = T.intercalate "\n" [errorMessage "" mErr "", errorMessage "" "" iErr]
errorMessage pErr mErr iErr = T.intercalate "\n" [errorMessage pErr "" "", errorMessage "" mErr "", errorMessage "" "" iErr]

dcUsage :: Message -> DiscordHandler ()
dcUsage = reportError "You need to have 2 commas, text before the first comma is mon1 and the options for that mon, text between the two commas is the move used by mon1 and the settings of the environment, last section is mon2 and their options."

parseDC :: T.Text -> Either T.Text (T.Text, T.Text, T.Text)
parseDC t = if length st == 3 then Right (t3 st) else Left "Message given wasn't in the correct format!"
  where
    st = T.splitOn "," t
    t3 [x, y, z] = (x, y, z)
    t3 _ = error "Unreachable situation"
