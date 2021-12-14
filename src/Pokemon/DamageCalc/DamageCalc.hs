{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pokemon.DamageCalc.DamageCalc where

import Control.Arrow
import Control.Monad.Identity (Identity (runIdentity), IdentityT (runIdentityT))
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char (toLower)
import Data.Function
import Data.Functor ((<&>))
import Data.List.Utility (hasAny)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.StatMultiplier
import qualified Data.Text as T
import GHC.Float (roundTo)
import Pokemon.DamageCalc.Functions
import Pokemon.DamageCalc.Types
import Pokemon.Functions
import Pokemon.TypeMatchups
import Pokemon.Types
import Prelude hiding (log)

runCalc :: DCS -> (CalcResult, Log)
runCalc dcs = runCalc' dcs calcDamage

runCalc' :: DCS -> Calc CalcResult -> (CalcResult, Log)
runCalc' dcs (Calc m) = runIdentity (runWriterT (runReaderT m dcs))

type Log = M.Map String String

newtype Calc a = Calc (ReaderT DCS (WriterT Log Identity) a)
  deriving (Functor, Applicative, Monad, MonadReader DCS, MonadWriter Log)

log :: String -> String -> Calc ()
log k a = tell (M.singleton k a)

logIf :: Bool -> String -> String -> Calc ()
logIf b k a = if b then tell (M.singleton k a) else pure ()

calcDamage :: Calc CalcResult
calcDamage =
  baseDamage
    >>= targetsMultiplier
    >>= weatherMultiplier
    >>= criticalHitMultiplier
    >>= randomMultiplier
    >>= stabMultiplier
    >>= typeEffectivenessMultiplier
    >>= screensMultiplier
    >>= minimizeMultiplier
    >>= invulnerableMultiplier
    >>= abilityMultiplier
    >>= itemMultiplier
    >>= multiHitMultiplier
    >>= parentalBond
    >>= toCalc

baseDamage :: Calc Int
baseDamage = do
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  move <- getMove
  env <- getEnvironment
  let lvl = epLevel attacker
      baseBp = getBp move attacker defender env
      terrainMultiplier = if isGrounded attacker || (isMisty env && isGrounded defender) then getTerrainMultiplier env moveType else 1
      abilityMultiplier = getAbilityMultiplier (epAbility attacker) move attacker defender env
      moveMultiplier = getMoveMultiplier attacker defender move env
      bpMultiplier = moveMultiplier * abilityMultiplier * terrainMultiplier
      moveCategory = getEMCategory attacker move attackerStats (epMultiplier attacker) defenderStats (epMultiplier defender)
      moveType = getMoveType attacker defender env move
      attackerStats = getEffectiveStats attacker
      defenderStats = getEffectiveStats defender
      attackMultipliers = getMultipliers defender (epMultiplier attacker)
      defenseMultipliers = getMultipliers attacker (epMultiplier defender)
      atkMultiplier = getAttackStatMultiplier (epAbility attacker) move attacker defender env
      spaMultiplier = getSpecialStatMultiplier (epAbility attacker) move attacker defender env
      defMultiplier = getDefenseStatMultiplier (epAbility defender) move attacker defender env
      spdMultiplier = getSpecialDefenseStatMultiplier (epAbility defender) move attacker defender env
      evioliteMult = if canUseItem defender env && hasItem "eviolite" defender && epNfe defender then 1.5 else 1
      assaultvestMult = if canUseItem defender env && hasItem "assaultvest" defender then 1.5 else 1
      sandMult = if hasWeather SANDSTORM env && ROCK `elem` getPokemonType defender then 1.5 else 1
      batteryMult = if battery env then 1.3 else 1
      powerMult = if powerspot env then 1.3 else 1
      choiceSpecs = if canUseItem attacker env && hasItem "choicespecs" attacker then 1.5 else 1
      choiceBand = if canUseItem attacker env && hasItem "choiceband" attacker then 1.5 else 1
      burned = if isBurned attacker && epAbility attacker /= "guts" && emName move /= "facade" then 0.5 else 1
      deepseatooth = if hasItem "deepseatooth" attacker && epName attacker == "clamperl" then 2 else 1
      deepseascale = if hasItem "deepseascale" defender && epName attacker == "clamperl" then 2 else 1
      atkMult = atkMultiplier * powerMult * burned * choiceBand
      spaMult = spaMultiplier * batteryMult * choiceSpecs * deepseatooth
      defMult = defMultiplier * evioliteMult
      spdMult = spdMultiplier * assaultvestMult * evioliteMult * sandMult * deepseascale
      (atk, statAMult, aMult, atkS) = getAttackAndMult (emName move) moveCategory (attackerStats, attackMultipliers) (defenderStats, defenseMultipliers) (atkMult, spaMult)
      (def, statDMult, dMult, defS) = getDefenseAndMult (emName move) moveCategory (attackerStats, attackMultipliers) (applyWonderRoom env defenderStats, defenseMultipliers) (defMult, spdMult)
      a = fromIntegral atk * (if epAbility attacker == "unaware" || epAbility defender == "unaware" then 1 else getMultiplier statAMult) * aMult
      d = fromIntegral def * (if epAbility attacker == "unaware" || epAbility defender == "unaware" then 1 else getMultiplier statDMult) * dMult
      bp = fromIntegral baseBp * bpMultiplier
      natureEffect = getNatureEffect (getStat (map toLower atkS)) (epNature attacker)
      defNe = getNatureEffect (getStat (map toLower defS)) (epNature defender)
  log "base bp" (show baseBp)
  log "bp" (show bp)
  log "atkS" atkS
  log "defS" defS
  log "atk" (show a)
  log "def" (show d)
  log "atkEv" (if atkS == "Atk" then (show . atkEv . epEvs) attacker else (show . spaEv . epEvs) attacker)
  log "defEv" (if defS == "Def" then (show . defEv . epEvs) defender else (show . spdEv . epEvs) attacker)
  log "attacker" ((T.unpack . epName) attacker)
  log "defender" ((T.unpack . epName) defender)
  log "move" ((T.unpack . emName) move)
  log "hpEv" ((show . hpEv . epEvs) defender)
  logIf (terrainMultiplier /= 1) "terrain" (fromMaybe "" (activeTerrain env <&> show))
  logIf (abilityMultiplier > 1) "atkAbility" ((T.unpack . epAbility) attacker)
  logIf (atkMultiplier > 1) "atkAbility" ((T.unpack . epAbility) attacker)
  logIf (spaMultiplier > 1) "atkAbility" ((T.unpack . epAbility) attacker)
  logIf (defMultiplier > 1) "defAbility" ((T.unpack . epAbility) defender)
  logIf (spdMultiplier > 1) "defAbility" ((T.unpack . epAbility) defender)
  logIf (powerspot env && moveCategory == PHYSICAL) "powerspot" "Powerspot boosted"
  logIf (battery env && moveCategory == SPECIAL) "battery" "Battery boosted"
  logIf ((emName move == "pursuit" || epAbility attacker == "stakeout") && switchingOut env) "switching" "switching boosted"
  logIf (evioliteMult /= 1) "defItem" "Eviolite"
  logIf (assaultvestMult /= 1) "defItem" "Assault Vest"
  logIf (sandMult /= 1) "weather" "Sand"
  logIf (choiceSpecs /= 1) "item" "Choice Specs"
  logIf (choiceBand /= 1) "item" "Choice Band"
  logIf (burned /= 1) "status" "burned"
  logIf (deepseatooth /= 1) "item" "Deep Sea Tooth"
  logIf (deepseascale /= 1) "defItem" "Deep Sea Scale"
  log "atkM" (show statAMult)
  log "defM" (show statDMult)
  log "ne" (toShowdownRep natureEffect)
  log "defNe" (toShowdownRep defNe)
  return $ floor ((((2 * fromIntegral lvl) / 5 + 2) * bp * (a / d)) / 50 + 2)

-- return $ div ((div (2 * lvl) 5 + 2) * bp * div atk def) 50 + 2

targetsMultiplier :: Int -> Calc Int
targetsMultiplier dmg = do
  log "base damage" (show dmg)
  env <- getEnvironment
  move <- getMove
  let mult = if isDoubleBattle env && (toId . emName) move `elem` multiTargetMoves then 0.75 else 1
  multiply dmg mult

weatherMultiplier :: Int -> Calc Int
weatherMultiplier dmg = do
  log "After targets multiplier" (show dmg)
  env <- getEnvironment
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  move <- getMove
  let moveType = getMoveType attacker defender env move
      weatherMult = fromMaybe 1 (activeWeather env <&> getWeatherMult moveType)
      atkAbility = toId . epAbility $ attacker
      defAbility = toId . epAbility $ defender
      defItem = fromMaybe "" (epItem defender <&> iName)
      mult = if atkAbility == "cloudnine" || defAbility == "cloudnine" || defItem == "utilityumbrella" || atkAbility == "airlock" || defAbility == "airlock" then 1 else weatherMult
  logIf (weatherMult /= 1) "weather" (fromMaybe "" (activeWeather env <&> show))
  multiply dmg mult

criticalHitMultiplier :: Int -> Calc Int
criticalHitMultiplier dmg = do
  log "After weather multiplier" (show dmg)
  env <- getEnvironment
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  move <- getMove
  let defAbility = toId . epAbility $ defender
      isCrit = defAbility /= "battlearmor" && defAbility /= "shellarmor" && (crit env || emWillCrit move || willCrit attacker defender)
      mult = if isCrit then 1.5 else 1
  logIf (mult /= 1) "crit" "on a critical hit"
  multiply dmg mult

randomMultiplier :: Int -> Calc (Int, Int)
randomMultiplier dmg = log "After crit multiplier" (show dmg) >> pure (fromIntegral dmg *// 0.85, dmg)

stabMultiplier :: (Int, Int) -> Calc (Int, Int)
stabMultiplier dmg = do
  log "After random multiplier" (show dmg)
  move <- getMove
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  env <- getEnvironment
  let atkAbility = toId . epAbility $ attacker
      mType = getMoveType attacker defender env move
      attackerType = getPokemonType attacker
      mult
        | atkAbility == "adaptability" && hasAny mType attackerType = 2
        | atkAbility == "protean" = 1.5
        | atkAbility == "libero" = 1.5
        | hasAny mType attackerType = 1.5
        | otherwise = 1
  log "attackertype" (show attackerType)
  logIf (atkAbility == "protean" && not (hasAny mType attackerType)) "atkAbility" "Protean"
  logIf (atkAbility == "libero" && not (hasAny mType attackerType)) "atkAbility" "Libero"
  logIf (atkAbility == "adaptability" && hasAny mType attackerType) "atkAbility" "Adaptability"
  multiply2 dmg mult

typeEffectivenessMultiplier :: (Int, Int) -> Calc (Int, Int)
typeEffectivenessMultiplier dmg = do
  log "after stab multiplier" (show dmg)
  move <- getMove
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  env <- getEnvironment
  let moveType = getMoveType attacker defender env move
      atkAbility = toId . epAbility $ attacker
      defAbility = toId . epAbility $ defender
      defenderType = getPokemonType defender
      defenderItem = fromMaybe "" (epItem defender <&> iName)
      moveName = toId . emName $ move
      tms = map getMatchup defenderType
      weatherBool = atkAbility `elem` ["cloudnine", "airlock"] || defAbility `elem` ["cloudnine", "airlock"]
      tms' =
        map
          ( enrichTmWithAbility (toId . epAbility $ attacker) (toId . epAbility $ defender) (toId . emName $ move)
              >>> updateTmWithItem defenderItem (magicRoom env)
              >>> enrichTmWithWeather (activeWeather env) weatherBool (getPokemonType defender)
              >>> enrichTmWithEnv env
              >>> thousandArrows moveName defenderType (emTimesUsed move)
          )
          tms
      tm = foldl (<>) mempty tms'
      ars = map (getDefenseRelation (defenseM tm)) moveType
      tintedLensMult = if mult < 1 && hasAbility "tintedlens" attacker then 2 else 1
      mult = toMultiplier $ foldl (<>) Neutral ars
  log "movetype" (show moveType)
  log "defender type" (show defenderType)
  log "ar" (show ars)
  multiply2 dmg (mult * tintedLensMult)

screensMultiplier :: (Int, Int) -> Calc (Int, Int)
screensMultiplier dmg = do
  log "After type effectiveness multiplier" (show dmg)
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  move <- getMove
  env <- getEnvironment
  let screen = screens env
      attackerStats = getEffectiveStats attacker
      defenderStats = getEffectiveStats defender
      attackMultipliers = getMultipliers defender (epMultiplier attacker)
      defenseMultipliers = getMultipliers attacker (epMultiplier defender)
      categoryMove = getEMCategory attacker move attackerStats attackMultipliers defenderStats defenseMultipliers
      moveName = emName move
      mult =
        if toId moveName `notElem` ["psychicfangs", "brickbreak", "gmaxwindrage"] && not (emWillCrit move || crit env || willCrit attacker defender)
          then getScreenMultiplier screen (toId . epAbility $ attacker) categoryMove
          else 1
  logIf (mult /= 1) "screens" "through Screen"
  multiply2 dmg mult

minimizeMultiplier :: (Int, Int) -> Calc (Int, Int)
minimizeMultiplier dmg = do
  log "after screens multiplier" (show dmg)
  env <- getEnvironment
  move <- getMove
  let moves = ["bodyslam", "dragonrush", "flyingpress", "heatcrash", "heavyslam", "maliciousmoonsault", "steamroller", "stomp"]
      mult =
        if (toId . emName) move `elem` moves && isMinimized env
          then 2
          else 1
  multiply2 dmg mult

invulnerableMultiplier :: (Int, Int) -> Calc (Int, Int)
invulnerableMultiplier dmg = do
  log "after minimize multiplier" (show dmg)
  env <- getEnvironment
  attacker <- getAttackingPokemon
  move <- getMove
  let moves = ["earthquake", "magnitude", "gust", "twister", "thunder", "skyuppercut", "smackdown"]
      mult
        | (toId . emName) move `elem` moves && isInvulnerable env = 2
        | (toId . epAbility) attacker == "noguard" = 1
        | isInvulnerable env = 0
        | otherwise = 1
  multiply2 dmg mult

abilityMultiplier :: (Int, Int) -> Calc (Int, Int)
abilityMultiplier dmg = do
  log "after invulnerability multiplier" (show dmg)
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  move <- getMove
  env <- getEnvironment
  let moveType = getMoveType attacker defender env move
      defenderType = getPokemonType defender
      atkAbility = toId . epAbility $ attacker
      defAbility = toId . epAbility $ defender
      ar = getTypeMatchup moveType defenderType
      moveCategory = emCategory move
      movePrio = getMovePriority attacker defender move env
      atkMult = if defAbility == "neutralizinggas" then 1 else getAttackAbilityMultiplier atkAbility attacker defender move env moveType moveCategory ar
      defMult =
        if (atkAbility == "neutralizinggas" || defAbility /= "prismarmor") && (atkAbility `elem` abilityIgnoringAbilities || (toId . emName) move `elem` movesThatIgnoreAbilities)
          then 1
          else getDefensiveAbilityMultiplier defAbility defender move {emPriority = movePrio} moveType moveCategory ar
  logIf (atkMult /= 1) "atkAbility" (T.unpack atkAbility)
  logIf (defMult /= 1) "defAbility" (T.unpack defAbility)
  multiply2 dmg (atkMult * defMult)

itemMultiplier :: (Int, Int) -> Calc (Int, Int)
itemMultiplier dmg = do
  log "After ability multiplier" (show dmg)
  move <- getMove
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  env <- getEnvironment
  let moveType = getMoveType attacker defender env move
      defenderType = getPokemonType defender
      typeMatchup = getTypeMatchup moveType defenderType
      oMult = getOffenseItemMultiplier (toId . epName $ attacker) move moveType typeMatchup (fromMaybe "" (epItem attacker <&> iName))
      dMult = getDefenseItemMultiplier moveType typeMatchup (fromMaybe "" (epItem attacker <&> iName)) (fromMaybe "" (epItem defender <&> iName)) (toId . epAbility $ attacker, toId . epAbility $ defender)
  logIf (oMult /= 1) "item" (fromMaybe "" (epItem attacker <&> (show . iName)))
  logIf (dMult /= 1) "defItem" (fromMaybe "" (epItem defender <&> (show . iName)))
  multiply2 dmg (if magicRoom env then 1 else oMult * dMult)

multiHitMultiplier :: (Int, Int) -> Calc (Int, Int)
multiHitMultiplier dmg = do
  log "after item multiplier" (show dmg)
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  move <- getMove
  env <- getEnvironment
  let hits = emHits move
      atkAbility = epAbility attacker
      defAbility = epAbility defender
      hasHalved = (defAbility `elem` ["multiscale", "shadowshield"] && epHPPercentage defender == 100) && (atkAbility /= "neutralizinggas" || atkAbility `notElem` abilityIgnoringAbilities || (toId . emName) move `notElem` movesThatIgnoreAbilities)
      totalDmg = both (\dmg -> if hasHalved then dmg + 2 * (hits - 1) * dmg else hits * dmg) dmg
  return totalDmg

parentalBond :: (Int, Int) -> Calc (Int, Int)
parentalBond dmg = do
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  move <- getMove
  env <- getEnvironment
  let hits = emHits move
      parentalBondActive = hasAbility "parentalbond" attacker && hits == 1 && emName move `notElem` multiTargetMoves
      defAbility = epAbility defender
      atkAbility = epAbility attacker
      hasHalved = (defAbility `elem` ["multiscale", "shadowshield"] && epHPPercentage defender == 100) && (atkAbility /= "neutralizinggas" || atkAbility `notElem` abilityIgnoringAbilities || (toId . emName) move `notElem` movesThatIgnoreAbilities)
      totalDmg = both (getTotalDmg parentalBondActive hasHalved) dmg
  logIf (totalDmg /= dmg) "atkAbility" "Parental Bond"
  log "hasHalved" (show hasHalved)
  return totalDmg
  where
    getTotalDmg pa hh dmg
      | pa && hh = dmg + floor (2 * 0.25 * fromIntegral dmg)
      | pa = dmg + floor (0.25 * fromIntegral dmg)
      | otherwise = dmg

both :: (a -> b) -> (a, a) -> (b, b)
both f (a1, a2) = (f a1, f a2)

toCalc :: (Int, Int) -> Calc CalcResult
toCalc hp@(minHp, maxHp) = do
  log "After parental bond calc" (show hp)
  defender <- getDefendingPokemon
  let hpS = hpStat (getEffectiveStats defender)
  return $ CalcResult hp (round' (on (/) fromIntegral minHp hpS) 3, round' (on (/) fromIntegral maxHp hpS) 3)

round' :: Double -> Integer -> Double
round' num sg = (fromIntegral . round $ num * f) / f
  where
    f = 10 ^ sg

-- HELPER FUNCTIONS

getEnvironment :: Calc Environment
getEnvironment = reader f
  where
    f (DCS env _ _ _) = env

getAttackingPokemon :: Calc EffectivePokemon
getAttackingPokemon = reader f
  where
    f (DCS _ p _ _) = p

getDefendingPokemon :: Calc EffectivePokemon
getDefendingPokemon = reader f
  where
    f (DCS _ _ p _) = p

getMove :: Calc EffectiveMove
getMove = reader f
  where
    f (DCS _ _ _ move) = move

multiply :: Int -> Double -> Calc Int
multiply dmg mult = return (fromIntegral dmg *// mult)

multiply2 :: (Int, Int) -> Double -> Calc (Int, Int)
multiply2 (min, max) mult = return (fromIntegral min *// mult, fromIntegral max *// mult)

hasAbility :: T.Text -> EffectivePokemon -> Bool
hasAbility a ep = epAbility ep == a