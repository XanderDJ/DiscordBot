module Pokemon.DamageCalc.DamageCalc where

import Control.Arrow
import Control.Monad.Reader
import Data.Functor ((<&>))
import Data.List.Utility (hasAny)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.StatMultiplier
import qualified Data.Text as T
import Pokemon.DamageCalc.Functions
import Pokemon.DamageCalc.Types
import Pokemon.Functions
import Pokemon.TypeMatchups
import Pokemon.Types

runCalc :: DCS -> (Int, Int)
runCalc = runReader calcDamage

calcDamage :: Reader DCS (Int, Int)
calcDamage =
  baseDamage
    >>= targetsMultiplier
    >>= weatherMultiplier
    >>= criticalHitMultiplier
    >>= stabMultiplier
    >>= typeEffectivenessMultiplier
    >>= screensMultiplier
    >>= minimizeMultiplier
    >>= invulnerableMultiplier
    >>= abilityMultiplier
    >>= itemMultiplier
    >>= otherMultiplier
    >>= randomMultiplier

baseDamage :: Reader DCS Int
baseDamage = do
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  move <- getMove
  let bp = getBp move attacker defender
      moveCategory = emCategory move
      atk = fromIntegral (getAttackStat (epItem attacker) moveCategory attacker) *// (getMultiplier . getAttackMultiplier moveCategory . epMultiplier) attacker
      def = fromIntegral (getDefenseStat (epItem defender) moveCategory defender) *// (getMultiplier . getDefenseMultiplier moveCategory . epMultiplier) defender
      lvl = epLevel attacker
      getAttackMultiplier PHYSICAL = atkM
      getAttackMultiplier SPECIAL = spaM
      getAttackMultiplier OTHER = atkM
      getDefenseMultiplier PHYSICAL = defM
      getDefenseMultiplier SPECIAL = spdM
      getDefenseMultiplier OTHER = spdM
  return $ div ((div (2 * lvl) 5 + 2) * bp * div atk def) 50 + 2

targetsMultiplier :: Int -> Reader DCS Int
targetsMultiplier dmg = do
  env <- getEnvironment
  move <- getMove
  let mult = if isDoubleBattle env && (toId . emName) move `elem` multiTargetMoves then 0.75 else 1
  multiply dmg mult

weatherMultiplier :: Int -> Reader DCS Int
weatherMultiplier dmg = do
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
  multiply dmg mult

criticalHitMultiplier :: Int -> Reader DCS Int
criticalHitMultiplier dmg = do
  env <- getEnvironment
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  move <- getMove
  let 
      defAbility = toId . epAbility $ defender
      isCrit = defAbility /= "battlearmor" && defAbility /= "shellarmor" && (crit env || emWillCrit move)
      mult = if isCrit then 1.5 else 1
  multiply dmg mult

stabMultiplier :: Int -> Reader DCS Int
stabMultiplier dmg = do
  move <- getMove
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  env <- getEnvironment
  let atkAbility = toId . epAbility $ attacker
      mType = getMoveType attacker defender env move
      attackerType = epTyping attacker
      mult
        | atkAbility == "adaptability" && hasAny mType attackerType = 2
        | hasAny mType attackerType = 1.5
        | otherwise = 1
  multiply dmg mult

typeEffectivenessMultiplier :: Int -> Reader DCS Int
typeEffectivenessMultiplier dmg = do
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
      tm' = fromMaybe mempty (getCombinedMatchup defenderType)
      weatherBool = atkAbility `elem` ["cloudnine", "airlock"] || defAbility `elem` ["cloudnine", "airlock"]
      tm =
        ( enrichTmWithAbility (toId . epAbility $ attacker) (toId . epAbility $ defender) (toId . emName $ move)
            >>> updateTmWithItem defenderItem (magicRoom env)
            >>> enrichTmWithWeather (activeWeather env)  weatherBool moveType
            >>> thousandArrows moveName defenderType (emTimesUsed move)
        )
          tm'
      ars = map (getDefenseRelation (defenseM tm)) moveType
      mult = toMultiplier $ foldl (<>) Neutral ars
  multiply dmg mult

screensMultiplier :: Int -> Reader DCS Int
screensMultiplier dmg = do
  attacker <- getAttackingPokemon
  move <- getMove
  env <- getEnvironment
  let screen = screens env
      categoryMove = emCategory move
      moveName = emName move
      mult =
        if toId moveName `notElem` ["psychicfangs", "brickbreak", "gmaxwindrage"]
          then getScreenMultiplier screen (toId . epAbility $ attacker) (emWillCrit move || crit env) categoryMove
          else 1
  multiply dmg mult

minimizeMultiplier :: Int -> Reader DCS Int
minimizeMultiplier dmg = do
  env <- getEnvironment
  move <- getMove
  let moves = ["bodyslam", "dragonrush", "flyingpress", "heatcrash", "heavyslam", "maliciousmoonsault", "steamroller", "stomp"]
      mult =
        if (toId . emName) move `elem` moves && isMinimized env
          then 2
          else 1
  multiply dmg mult

invulnerableMultiplier :: Int -> Reader DCS Int
invulnerableMultiplier dmg = do
  env <- getEnvironment
  attacker <- getAttackingPokemon
  move <- getMove
  let moves = ["earthquake", "magnitude", "gust", "twister", "thunder", "skyuppercut", "smackdown"]
      mult
        | (toId . emName) move `elem` moves && isInvulnerable env = 2
        | (toId . epAbility) attacker == "noguard" = 1
        | otherwise = 0
  multiply dmg mult

abilityMultiplier :: Int -> Reader DCS Int
abilityMultiplier dmg = do
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
  multiply dmg (atkMult * defMult)



itemMultiplier :: Int -> Reader DCS Int
itemMultiplier dmg = do
  move <- getMove
  attacker <- getAttackingPokemon
  defender <- getDefendingPokemon
  env <- getEnvironment
  let moveType = getMoveType attacker defender env move
      defenderType = getPokemonType defender
      typeMatchup = getTypeMatchup moveType defenderType
      mult = getItemMultiplier (toId . epName $ attacker) move moveType typeMatchup (fromMaybe "" (epItem attacker <&> iName)) (toId . epAbility $ attacker, toId . epAbility $ defender)
  multiply dmg (if magicRoom env then 1 else mult)

randomMultiplier :: Int -> Reader DCS (Int, Int)
randomMultiplier dmg = pure (fromIntegral dmg *// 0.85, dmg)

-- HELPER FUNCTIONS

getEnvironment :: Reader DCS Environment
getEnvironment = reader f
  where
    f (DCS env _ _ _) = env

getAttackingPokemon :: Reader DCS EffectivePokemon
getAttackingPokemon = reader f
  where
    f (DCS _ p _ _) = p

getDefendingPokemon :: Reader DCS EffectivePokemon
getDefendingPokemon = reader f
  where
    f (DCS _ _ p _) = p

getMove :: Reader DCS EffectiveMove
getMove = reader f
  where
    f (DCS _ _ _ move) = move

otherMultiplier :: Int -> Reader DCS Int
otherMultiplier currentDmg = undefined

multiply :: Int -> Double -> Reader DCS Int
multiply dmg mult = return (fromIntegral dmg *// mult)