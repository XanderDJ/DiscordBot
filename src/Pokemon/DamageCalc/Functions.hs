{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Pokemon.DamageCalc.Functions where

import qualified Codec.Xlsx.Types.DataValidation as L
import Control.Arrow ((>>>))
import Data.Function (on)
import Data.Functor ((<&>))
import qualified Data.List as L
import Data.List.Utility (hasAny)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Sum (Sum, getSum))
import Data.StatMultiplier (StatMultiplier, getMultiplier, getPositiveBoosts)
import qualified Data.Text as T
import Pokemon.DamageCalc.Types
import Pokemon.Functions
import Pokemon.TypeMatchups
import Pokemon.Types
import PokemonDB.Types (ItemT (isBerry), MoveT (moveType))
import Text.Read (readMaybe)

getAttackAndMult :: T.Text -> AttackType -> (EffectiveStats, Multipliers) -> (EffectiveStats, Multipliers) -> (Double, Double) -> (Int, StatMultiplier, Double)
getAttackAndMult "bodypress" _ (es, em) _ (atkMult, spaMult) = (defStat es, defM em, atkMult)
getAttackAndMult "foulplay" _ _ (es, em) (atkMult, spaMult) = (atkStat es, atkM em, atkMult)
getAttackAndMult move SPECIAL (es, em) _ (atkMult, spaMult) = (spaStat es, spaM em, spaMult)
getAttackAndMult move PHYSICAL (es, em) _ (atkMult, spaMult) = (atkStat es, atkM em, atkMult)
getAttackAndMult move OTHER (es, em) _ (atkMult, spaMult) = (0, 0, 0)

getDefenseAndMult :: T.Text -> AttackType -> (EffectiveStats, Multipliers) -> (EffectiveStats, Multipliers) -> (Double, Double) -> (Int, StatMultiplier, Double)
getDefenseAndMult "psystrike" _ _ (es, em) (defMult, spdMult) = (defStat es, defM em, defMult)
getDefenseAndMult "psyshock" _ _ (es, em) (defMult, spdMult) = (defStat es, defM em, defMult)
getDefenseAndMult "secretsword" _ _ (es, em) (defMult, spdMult) = (defStat es, defM em, defMult)
getDefenseAndMult "naturepower" _ _ (es, em) (defMult, spdMult) = (spdStat es, spdM em, spdMult)
getDefenseAndMult "sacredsword" _ _ (es, _) (defMult, spdMult) = (defStat es, 0, defMult)
getDefenseAndMult "darkestlariat" _ _ (es, _) (defMult, spdMult) = (defStat es, 0, defMult)
getDefenseAndMult "chipaway" _ _ (es, _) (defMult, spdMult) = (defStat es, 0, defMult)
getDefenseAndMult "wickedblow" _ _ (es, _) (defMult, spdMult) = (defStat es, 0, defMult)
getDefenseAndMult "surgingstrikes" _ _ (es, _) (defMult, spdMult) = (defStat es, 0, defMult)
getDefenseAndMult _ PHYSICAL _ (es, em) (defMult, spdMult) = (defStat es, defM em, defMult)
getDefenseAndMult _ SPECIAL _ (es, em) (defMult, spdMult) = (spdStat es, spdM em, spdMult)
getDefenseAndMult _ OTHER _ (es, em) (defMult, spdMult) = (defStat es, defM em, defMult)


applyWonderRoom :: Environment -> EffectiveStats -> EffectiveStats 
applyWonderRoom Env {wonderRoom = True} ES {..} = ES hpStat atkStat spdStat spaStat defStat speStat
applyWonderRoom _ es = es

getMultipliers :: EffectivePokemon -> Multipliers -> Multipliers
getMultipliers EP {epAbility = "unaware"} sm = Multipliers 0 0 0 0 0
getMultipliers _ sm = sm

facadeMultiplier :: EffectiveMove -> EffectivePokemon -> Double
facadeMultiplier EM {emName = "facade"} EP {epStatus = Just x} = 2
facadeMultiplier _ _ = 1

getMoveMultiplier :: EffectivePokemon -> EffectivePokemon -> EffectiveMove -> Environment -> Double
getMoveMultiplier attacker defender em@EM {emName = "fishiousrend"} env = let moveOrder = getMoveOrder attacker defender em env in if moveOrder == FIRST then 2 else 1
getMoveMultiplier attacker defender em@EM {emName = "boltbeak"} env = let moveOrder = getMoveOrder attacker defender em env in if moveOrder == FIRST then 2 else 1
getMoveMultiplier _ _ em@EM {emName = "avalanche"} env = if hitBefore env then 2 else 1
getMoveMultiplier _ _ em@EM {emName = "assurance"} env = if hitBefore env then 2 else 1
getMoveMultiplier _ EP {epHPPercentage = rem} EM {emName = "brine"} env = if rem < 50 then 2 else 1
getMoveMultiplier _ _ EM {emName = "pursuit"} Env {switchingOut = switchingOut} = if switchingOut then 2 else 1
getMoveMultiplier EP {epItem = Nothing} _ EM {emName = "acrobatics"} _ = 2
getMoveMultiplier EP {epStatsLowered = True} _ EM {emName = "lashout"} _ = 2
getMoveMultiplier _ _ EM {emName = "mistyexplosion"} Env {activeTerrain = Just MISTY} = 1.5
getMoveMultiplier _ _ EM {emName = "risingvoltage"} Env {activeTerrain = Just ELECTRIC_T} = 2
getMoveMultiplier _ _ EM {emName = "expandingforce"} Env {activeTerrain = Just PSYCHIC_T} = 2
getMoveMultiplier _ EP {epStatus = Just PARALYZED} EM {emName = "smellingsalts"} _ = 2
getMoveMultiplier _ EP {epStatus = Just x} EM {emName = "hex"} _ = 2
getMoveMultiplier _ EP {epStatus = Just POISONED} EM {emName = "venoshock"} _ = 2
getMoveMultiplier _ EP {epStatus = Just SLEEP} EM {emName = "wakeupslap"} _ = 2
getMoveMultiplier attacker defender move env = 1

getTerrainMultiplier :: Environment -> EffectiveMove -> Double
getTerrainMultiplier Env {activeTerrain = Just GRASSY} EM {emType = tipe} = if tipe == GRASS then 1.3 else 1
getTerrainMultiplier Env {activeTerrain = Just ELECTRIC_T} EM {emType = tipe} = if tipe == ELECTRIC then 1.3 else 1
getTerrainMultiplier Env {activeTerrain = Just PSYCHIC_T} EM {emType = tipe} = if tipe == PSYCHIC then 1.3 else 1
getTerrainMultiplier _ _ = 1

-- | AAMM == AttackAbilityMove
getAbilityMultiplier :: T.Text -> EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Double
getAbilityMultiplier _ _ _ EP {epAbility = "neutralizinggas"} _ = 1
getAbilityMultiplier "aerilate" EM {emName = moveName, emType = mType} _ _ _ = if moveName `notElem` ["naturalgift", "naturepower"] && NORMAL == mType then 1.2 else 1
getAbilityMultiplier "galvanize" EM {emName = moveName, emType = mType} _ _ _ = if moveName `notElem` ["naturalgift", "naturepower"] && NORMAL == mType then 1.2 else 1
getAbilityMultiplier "pixilate" EM {emName = moveName, emType = mType} _ _ _ = if moveName `notElem` ["naturalgift", "naturepower"] && NORMAL == mType then 1.2 else 1
getAbilityMultiplier "refrigerate" EM {emName = moveName, emType = mType} _ _ _ = if moveName `notElem` ["naturalgift", "naturepower"] && NORMAL == mType then 1.2 else 1
getAbilityMultiplier "sheerforce" EM {emHasSecondary = True} _ _ _ = 1.3
getAbilityMultiplier "technician" EM {emBp = bp} _ _ _ = if bp <= 60 then 1.5 else 1
getAbilityMultiplier "analytic" em attacker defender env =
  let order = getMoveOrder attacker defender em env
      moveName = emName em
   in if order == LAST && moveName `notElem` ["futuresight", "doomdesire"] then 1.3 else 1
getAbilityMultiplier "fairyaura" EM {emType = tipe} _ _ _ = if FAIRY == tipe then 1.33 else 1
getAbilityMultiplier "darkaura" EM {emType = tipe} _ _ _ = if DARK == tipe then 1.33 else 1
getAbilityMultiplier _ EM {emType = tipe} EP {epFlashFire = True} _ _ = if FIRE == tipe then 1.5 else 1
getAbilityMultiplier "ironfist" EM {emPunch = True} _ _ _ = 1.2
getAbilityMultiplier "punkrock" EM {emSound = True} _ _ _ = 1.3
getAbilityMultiplier "reckless" EM {emRecoil = True} _ _ _ = 1.2
getAbilityMultiplier "sandforce" EM {emType = tipe} _ _ Env {activeWeather = Just SANDSTORM} = if tipe `elem` [STEEL, GROUND, ROCK] then 1.3 else 1
getAbilityMultiplier "steelworker" EM {emType = tipe} _ _ _ = if tipe == STEEL then 1.5 else 1
getAbilityMultiplier "steelyspirit" EM {emType = tipe} _ _ _ = if tipe == STEEL then 1.5 else 1
getAbilityMultiplier "defeatist" _ EP {epHPPercentage = percentage} _ _ = if percentage < 50 then 0.5 else 1
getAbilityMultiplier "strongjaw" EM {emBite = True} _ _ _ = 1.5
getAbilityMultiplier _ EM {emType = tipe} _ EP {epAbility = "thickfat"} _ = if tipe `elem` [ICE, FIRE] then 0.5 else 1
getAbilityMultiplier "torrent" EM {emType = tipe} EP {epHPPercentage = percentage} _ _ = if tipe == WATER && percentage < 1 / 3 then 1.5 else 1
getAbilityMultiplier "blaze" EM {emType = tipe} EP {epHPPercentage = percentage} _ _ = if tipe == FIRE && percentage < 1 / 3 then 1.5 else 1
getAbilityMultiplier "overgrow" EM {emType = tipe} EP {epHPPercentage = percentage} _ _ = if tipe == GRASS && percentage < 1 / 3 then 1.5 else 1
getAbilityMultiplier "dragonsmaw" EM {emType = tipe} _ _ _ = if tipe == DRAGON then 1.5 else 1
getAbilityMultiplier "transistor" EM {emType = tipe} _ _ _ = if tipe == ELECTRIC then 1.5 else 1
getAbilityMultiplier "waterbubble" EM {emType = tipe} _ _ _ = if tipe == WATER then 2 else 1
getAbilityMultiplier "megalauncer" EM {emPulse = True} _ _ _ = 1.5
getAbilityMultiplier "stakeout" _ _ _ Env {switchingOut = True} = 2
getAbilityMultiplier ability move attacker defender environment = 1

getAttackStatMultiplier :: T.Text -> EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Double
getAttackStatMultiplier "hugepower" _ _ _ _ = 2
getAttackStatMultiplier "purepower" _ _ _ _ = 2
getAttackStatMultiplier "hustle" _ _ _ _ = 1.5
getAttackStatMultiplier "flowergift" _ _ _ Env {activeWeather = Just x} = if x `elem` [SUN, HEAVYSUN] then 1.5 else 1
getAttackStatMultiplier "gorillatactics" _ _ _ _ = 1.5
getAttackStatMultiplier "guts" _ EP {epStatus = Just x} _ _ = 1.5
getAttackStatMultiplier "toxic" _ EP {epStatus = Just POISONED} _ _ = 1.5
getAttackStatMultiplier _ _ _ _ _ = 1

getDefenseStatMultiplier :: T.Text -> EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Double
getDefenseStatMultiplier "furcoat" _ _ _ _ = 2
getDefenseStatMultiplier "grasspelt" _ _ _ Env {activeTerrain = Just GRASSY} = 1.5
getDefenseStatMultiplier "marvelscale" _ _ EP {epStatus = Just x} _ = 1.5
getDefenseStatMultiplier _ _ _ _ _ = 1

getSpecialStatMultiplier :: T.Text -> EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Double
getSpecialStatMultiplier "flareboost" _ EP {epStatus = Just BURN} _ _ = 1.5
getSpecialStatMultiplier _ _ _ _ _ = 1

getSpecialDefenseStatMultiplier :: T.Text -> EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Double
getSpecialDefenseStatMultiplier _ _ _ _ _ = 1

getMoveOrder :: EffectivePokemon -> EffectivePokemon -> EffectiveMove -> Environment -> MoveOrder
getMoveOrder attacker defender move env
  | prio > 0 = FIRST
  | prio < 0 = LAST
  | otherwise = if trickRoom env then flipOrder order else order
  where
    attackerSpeed = getSpeedAttacker attacker env
    defenderSpeed = getSpeedDefender defender env
    moveType = getMoveType attacker defender env move
    order = if attackerSpeed >= defenderSpeed then FIRST else LAST
    flipOrder FIRST = LAST
    flipOrder LAST = FIRST
    prio = getMovePriority attacker defender move env

getEMCategory :: EffectivePokemon -> EffectiveMove -> EffectiveStats -> Multipliers -> EffectiveStats -> Multipliers -> AttackType
getEMCategory EP {epLevel = level} EM {emName = "shellsidearm"} atkStats atkMults defStats defMults =
  let attackStat = getTotalStat (atkStat atkStats) (atkM atkMults)
      specialAttackStat = getTotalStat (spaStat atkStats) (spaM atkMults)
      defenseStat = getTotalStat (defStat defStats) (defM defMults)
      specialDefenseStat = getTotalStat (spdStat defStats) (spdM defMults)
   in getShellSideArmCat level attackStat specialAttackStat defenseStat specialDefenseStat
getEMCategory _ EM {emName = "photongeyser"} atkStats atkMults _ _ =
  if getTotalStat (atkStat atkStats) (atkM atkMults) > getTotalStat (spaStat atkStats) (spaM atkMults)
    then PHYSICAL
    else SPECIAL
getEMCategory _ em _ _ _ _ = emCategory em

getShellSideArmCat :: Integral a => a -> a -> a -> a -> a -> AttackType
getShellSideArmCat level atkStat spaStat defStat spdStat = if x > y then PHYSICAL else SPECIAL
  where
    x = div (div ((div (2 * level) 5 + 5) * 90 * atkStat) defStat) 50
    y = div (div ((div (2 * level) 5 + 5) * 90 * spaStat) spdStat) 50

getTotalStat :: (Integral b, Integral a) => a -> StatMultiplier -> b
getTotalStat stat boost = fromIntegral stat *// getMultiplier boost

getMovePriority :: EffectivePokemon -> EffectivePokemon -> EffectiveMove -> Environment -> Int
getMovePriority attacker defender move env = prio
  where
    moveType = getMoveType attacker defender env move
    prio
      | FLYING `elem` moveType && epHPPercentage attacker == 100 = 1 + emPriority move
      | (toId . epAbility) attacker == "stall" = -6
      | (toId . epAbility) attacker == "triage" && emDrain move = 3
      | otherwise = emPriority move

getSpeedAttacker :: EffectivePokemon -> Environment -> Int
getSpeedAttacker attacker env = if tailWind env then fromIntegral speed *// 2 else speed
  where
    speed = getSpeed attacker env

getSpeedDefender :: EffectivePokemon -> Environment -> Int
getSpeedDefender = getSpeed

getSpeed :: EffectivePokemon -> Environment -> Int
getSpeed ep@EP {..} env = fromIntegral speedStat *// totalMultiplier
  where
    speedMultiplier = getSpeedMultiplier (toId epAbility) ep env
    boostMultiplier = (getMultiplier . speM) epMultiplier
    totalMultiplier = speedMultiplier * boostMultiplier
    natureEffect = getNatureEffect SPE epNature
    speedStat = calcStat epLevel (spdIv epIvs) (spdEv epEvs) natureEffect (findBaseStat epStats SPE)

getSpeedMultiplier :: T.Text -> EffectivePokemon -> Environment -> Double
getSpeedMultiplier "quickfeet" EP {epStatus = Just x} _ = 1.5
getSpeedMultiplier "slowstart" _ _ = 0.5
getSpeedMultiplier "slushrush" _ Env {activeWeather = Just HAIL} = 2
getSpeedMultiplier "swiftswim" _ Env {activeWeather = Just RAIN} = 2
getSpeedMultiplier "swiftswim" _ Env {activeWeather = Just HEAVYRAIN} = 2
getSpeedMultiplier "chlorophyll" _ Env {activeWeather = Just HEAVYSUN} = 2
getSpeedMultiplier "chlorophyll" _ Env {activeWeather = Just SUN} = 2
getSpeedMultiplier "surgesurfer" _ Env {activeTerrain = Just ELECTRIC_T} = 2
getSpeedMultiplier ability mon env = 1

getBp :: EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Int
getBp m = getBp' (toId . emName $ m) m

getBp' :: T.Text -> EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Int
getBp' "heavyslam" _ atk def _ = getWeightRatioPower atk def
getBp' "heatcrash" _ atk def _ = getWeightRatioPower atk def
getBp' "lowkick" _ _ def _ = getWeightPower (getWeight def)
getBp' "grassknot" _ _ def _ = getWeightPower (getWeight def)
getBp' "skydrop" EM {emBp = bp} _ def _ = if getWeight def >= 200 then 0 else bp
getBp' "gyroball" _ atk def env =
  let attackerSpeed = getSpeedAttacker atk env
      defenderSpeed = getSpeedDefender def env
   in min 150 (floor (25 * on (/) fromIntegral attackerSpeed defenderSpeed + 1))
getBp' "electroball" _ atk def env =
  let attackerSpeed = getSpeedAttacker atk env
      defenderSpeed = getSpeedDefender def env
   in getRatioPower (floor (on (/) fromIntegral attackerSpeed defenderSpeed))
getBp' "fling" _ atk _ _ = fromMaybe 0 (epItem atk <&> iFlingBp)
getBp' "flail" _ atk _ _ = let ratio = floor $ (epHPPercentage atk * 48) / 100 in getHpRatioPower ratio
getBp' "reversal" _ atk _ _ = let ratio = floor $ (epHPPercentage atk * 48) / 100 in getHpRatioPower ratio
getBp' "waterspout" _ EP {epHPPercentage = percentage} _ _ = max 1 (div (floor (percentage * 150)) 100)
getBp' "eruption" _ EP {epHPPercentage = percentage} _ _ = getHpPercentagePower percentage
getBp' "dragonenergy" _ EP {epHPPercentage = percentage} _ _ = getHpPercentagePower percentage
getBp' "powertrip" _ EP {epMultiplier = multipliers} _ _ = getMultipliersPower 20 multipliers
getBp' "storedpower" _ EP {epMultiplier = multipliers} _ _ = getMultipliersPower 20 multipliers
getBp' "return" _ _ _ _ = 102
getBp' "frustration" _ _ _ _ = 102
getBp' "punishment" _ _ EP {epMultiplier = multipliers} _ = getMultipliersPower 60 multipliers
getBp' "spitup" EM {emStockPile = sp, emBp = bp} _ _ _ = fromMaybe bp (sp >>= getStockPilePower)
getBp' "naturalgift" _ atk def env = fst (getNaturalGiftValues atk def env)
getBp' "naturepower" _ _ _ env = fst (getNaturePowerValues env)
getBp' "crushgrip" _ _ EP {epHPPercentage = percentage} _ = max 1 (div (round $ 120 * percentage) 100)
getBp' "furycutter" EM {emTimesUsed = tu} _ _ _ = min 160 (40 * (2 ^ tu))
getBp' "rollout" EM {emTimesUsed = tu} _ _ _ = 30 * (2 ^ min 4 tu)
getBp' "iceball" EM {emTimesUsed = tu} _ _ _ = 30 * (2 ^ min 4 tu)
getBp' "pikapapow" _ _ _ _ = 102
getBp' name em atk def env = emBp em

getStockPilePower :: Num a => a -> Maybe a
getStockPilePower sp = Just (100 * sp)

getMultipliersPower :: Int -> Multipliers -> Int
getMultipliersPower base Multipliers {..} = base + getSum x * 20
  where
    x = foldMap (Sum . getPositiveBoosts) [atkM, defM, spaM, spdM, speM]

getHpPercentagePower :: (Integral a1, RealFrac a2) => a2 -> a1
getHpPercentagePower percentage = max 1 (div (floor (percentage * 150)) 100)

getHpRatioPower :: Int -> Int
getHpRatioPower ratio
  | ratio >= 33 = 20
  | ratio >= 17 = 40
  | ratio >= 10 = 80
  | ratio >= 5 = 100
  | ratio >= 2 = 150
  | otherwise = 200

getWeightRatioPower :: EffectivePokemon -> EffectivePokemon -> Int
getWeightRatioPower atk def = getRatioPower (floor (on (/) (fromIntegral . getWeight) atk def))

getWeightPower :: Int -> Int
getWeightPower weight
  | weight >= 200 = 120
  | weight >= 100 = 100
  | weight >= 50 = 80
  | weight >= 25 = 60
  | weight >= 10 = 40
  | otherwise = 20

getWeight :: EffectivePokemon -> Int
getWeight EP {epWeight = baseWeight, epAbility = ability}
  | toId ability == "heavymetal" = 2 * baseWeight
  | toId ability == "lightmetal" = fromIntegral baseWeight *// 0.5
  | otherwise = baseWeight

getRatioPower :: Int -> Int
getRatioPower ratio
  | ratio > 5 = 120
  | ratio == 4 = 100
  | ratio == 3 = 80
  | ratio == 2 = 60
  | otherwise = 40

getEffectiveStats :: EffectivePokemon -> EffectiveStats
getEffectiveStats EP {..} = ES hp atk def spa spd spe
  where
    hp = calcStat epLevel (hpIv epIvs) (hpEv epEvs) (getNatureEffect HP epNature) (findBaseStat epStats HP)
    atk = calcStat epLevel (atkIv epIvs) (atkEv epEvs) (getNatureEffect ATK epNature) (findBaseStat epStats ATK)
    def = calcStat epLevel (defIv epIvs) (defEv epEvs) (getNatureEffect DEF epNature) (findBaseStat epStats DEF)
    spa = calcStat epLevel (spaIv epIvs) (spaEv epEvs) (getNatureEffect SPA epNature) (findBaseStat epStats SPA)
    spd = calcStat epLevel (spdIv epIvs) (spdEv epEvs) (getNatureEffect SPD epNature) (findBaseStat epStats SPD)
    spe = calcStat epLevel (speIv epIvs) (speEv epEvs) (getNatureEffect SPE epNature) (findBaseStat epStats SPE)

getDefensiveAbilityMultiplier :: T.Text -> EffectivePokemon -> EffectiveMove -> [Type] -> AttackType -> AttackRelation -> Double
getDefensiveAbilityMultiplier "bulletproof" _ EM {emBullet = isBullet} _ _ _ = if isBullet then 0 else 1
getDefensiveAbilityMultiplier "filter" _ _ _ _ ar = if ar `elem` [SuperEffective, StronglyEffective] then 0.75 else 1
getDefensiveAbilityMultiplier "solidrock" _ _ _ _ ar = if ar `elem` [SuperEffective, StronglyEffective] then 0.75 else 1
getDefensiveAbilityMultiplier "fluffy" _ EM {isContact = contact} typing _ _
  | contact && FIRE `elem` typing = 1
  | contact = 0.5
  | FIRE `elem` typing = 2
  | otherwise = 1
getDefensiveAbilityMultiplier "icescales" _ _ _ SPECIAL _ = 0.5
getDefensiveAbilityMultiplier "heatproof" _ _ typing _ _ = if FIRE `elem` typing then 0.5 else 1
getDefensiveAbilityMultiplier "multiscale" EP {epHPPercentage = percentage} _ _ _ _ = if percentage == 100 then 0.5 else 1
getDefensiveAbilityMultiplier "shadowshield" EP {epHPPercentage = percentage} _ _ _ _ = if percentage == 100 then 0.5 else 1
getDefensiveAbilityMultiplier "prismarmor" _ _ _ _ ar = if ar `elem` [SuperEffective, StronglyEffective] then 0.75 else 1
getDefensiveAbilityMultiplier "punkrock" _ EM {emSound = isSound} _ _ _ = if isSound then 0.5 else 1
getDefensiveAbilityMultiplier "soundproof" _ EM {emSound = isSound} _ _ _ = if isSound then 0 else 1
getDefensiveAbilityMultiplier "waterbubble" _ _ typing _ _ = if FIRE `elem` typing then 0.5 else 1
getDefensiveAbilityMultiplier "aurabreak" _ _ typing _ _ = if hasAny [FAIRY, DARK] typing then 3 / 4 else 1
getDefensiveAbilityMultiplier "damp" _ EM {emName = name} _ _ _ = if toId name `elem` ["selfdestruct", "explosion", "mindblown", "mistyexplosion"] then 0 else 1
getDefensiveAbilityMultiplier "dazzling" _ EM {emPriority = prio} _ _ _ = if prio > 0 then 0 else 1
getDefensiveAbilityMultiplier "dryskin" _ _ typing _ _ = if FIRE `elem` typing then 1.25 else 1
getDefensiveAbilityMultiplier "queenlymajesty" _ EM {emPriority = prio} _ _ _ = if prio > 0 then 0 else 1
getDefensiveAbilityMultiplier ab ep em typing cat super = 1

getAttackAbilityMultiplier :: T.Text -> EffectivePokemon -> EffectivePokemon -> EffectiveMove -> Environment -> [Type] -> AttackType -> AttackRelation -> Double
getAttackAbilityMultiplier "neuroforce" _ _ _ _ _ _ SuperEffective = 1.25
getAttackAbilityMultiplier ab attacker defender em env typing cat effectiveness = 1

getPokemonType :: EffectivePokemon -> [Type]
getPokemonType ep@EP {epName = "silvally"} = fromMaybe (epTyping ep) (epItem ep >>= iOnMemory >>= (fmap (: []) . readMaybe . T.unpack))
getPokemonType p@EP {epName = "arceus"} = fromMaybe (epTyping p) (epItem p >>= iOnPlate >>= (fmap (: []) . readMaybe . T.unpack))
getPokemonType ep = epTyping ep

getMoveBaseType :: EffectivePokemon -> EffectivePokemon -> T.Text -> Environment -> Type -> [Type]
getMoveBaseType _ _ "flyingpress" _ t = [t, FLYING]
getMoveBaseType p _ "multiattack" Env {magicRoom = isRoom} t =
  if not $ "silvally" `T.isInfixOf` (toId . epName) p || (toId . epAbility) p == "klutz" || isRoom
    then [t]
    else fromMaybe [t] (epItem p >>= iOnMemory >>= (fmap (: []) . readMaybe . T.unpack))
getMoveBaseType p _ "judgement" Env {magicRoom = isRoom} t =
  if not $ "arceus" `T.isInfixOf` (toId . epName) p || (toId . epAbility) p == "klutz" || isRoom
    then [t]
    else fromMaybe [t] (epItem p >>= iOnPlate >>= (fmap (: []) . readMaybe . T.unpack))
getMoveBaseType p _ "technoblast" Env {magicRoom = isRoom} t =
  if not $ "genesect" `T.isInfixOf` (toId . epName) p || (toId . epAbility) p == "klutz" || isRoom
    then [t]
    else fromMaybe [t] (epItem p >>= iOnDrive >>= (fmap (: []) . readMaybe . T.unpack))
getMoveBaseType EP {epTyping = typing} _ "revelationdance" _ _ = [head typing]
getMoveBaseType EP {epAbility = "normalize"} _ _ _ _ = [NORMAL]
getMoveBaseType _ _ _ _ t = [t]

getNaturePowerValues :: Environment -> (Int, Typing)
getNaturePowerValues Env {activeTerrain = terrain} =
  fromMaybe
    (80, [NORMAL])
    ( terrain
        >>= \case
          ELECTRIC_T -> Just (90, [ELECTRIC])
          PSYCHIC_T -> Just (90, [PSYCHIC])
          MISTY -> Just (95, [FAIRY])
          GRASSY -> Just (90, [GRASS])
    )

getNaturalGiftValues :: EffectivePokemon -> EffectivePokemon -> Environment -> (Int, Typing)
getNaturalGiftValues EP {epItem = item, epAbility = ability} EP {epAbility = a'} Env {magicRoom = isRoom} =
  fromMaybe
    (0, [NORMAL])
    ( item
        >>= \i ->
          if (not . iBerry) i || toId ability == "klutz" || isRoom || toId a' == "unnerve"
            then Just (0, [NORMAL])
            else case toId . iName $ i of
              "cheriberry" -> Just (60, [FIRE])
              "chestoberry" -> Just (60, [WATER])
              "pechaberry" -> Just (60, [ELECTRIC])
              "rawstberry" -> Just (60, [GRASS])
              "aspearberry" -> Just (60, [ICE])
              "leppaberry" -> Just (60, [FIGHTING])
              "oranberry" -> Just (60, [POISON])
              "persimberry" -> Just (60, [GROUND])
              "lumberry" -> Just (60, [FLYING])
              "sitrusberry" -> Just (60, [PSYCHIC])
              "figyberry" -> Just (60, [BUG])
              "wikiberry" -> Just (60, [ROCK])
              "magoberry" -> Just (60, [GHOST])
              "aguavberry" -> Just (60, [DRAGON])
              "iapapaberry" -> Just (60, [DARK])
              "razzberry" -> Just (60, [STEEL])
              "blukberry" -> Just (70, [FIRE])
              "nanabberry" -> Just (70, [WATER])
              "wepearberry" -> Just (70, [ELECTRIC])
              "pinapberry" -> Just (70, [GRASS])
              "pomegberry" -> Just (70, [ICE])
              "kelpsyberry" -> Just (70, [FIGHTING])
              "qualotberry" -> Just (70, [POISON])
              "hondewberry" -> Just (70, [GROUND])
              "grepaberry" -> Just (70, [FLYING])
              "tamatoberry" -> Just (70, [PSYCHIC])
              "cornnberry" -> Just (70, [BUG])
              "magostberry" -> Just (70, [ROCK])
              "rabutaberry" -> Just (70, [GHOST])
              "nomelberry" -> Just (70, [DRAGON])
              "spelonberry" -> Just (70, [DARK])
              "pamtreberry" -> Just (70, [STEEL])
              "watmelberry" -> Just (80, [FIRE])
              "durinberry" -> Just (80, [WATER])
              "belueberry" -> Just (80, [ELECTRIC])
              "occaberry" -> Just (60, [FIRE])
              "passhoberry" -> Just (60, [WATER])
              "wacanberry" -> Just (60, [ELECTRIC])
              "rindoberry" -> Just (60, [GRASS])
              "yacheberry" -> Just (60, [ICE])
              "chopleberry" -> Just (60, [FIGHTING])
              "kebiaberry" -> Just (60, [POISON])
              "shucaberry" -> Just (60, [GROUND])
              "cobaberry" -> Just (60, [FLYING])
              "payapaberry" -> Just (60, [PSYCHIC])
              "tangaberry" -> Just (60, [BUG])
              "chartiberry" -> Just (60, [ROCK])
              "kasibberry" -> Just (60, [GHOST])
              "habanberry" -> Just (60, [DRAGON])
              "colburberry" -> Just (60, [DARK])
              "babiriberry" -> Just (60, [STEEL])
              "chilanberry" -> Just (60, [NORMAL])
              "liechieberry" -> Just (80, [GRASS])
              "ganlonberry" -> Just (80, [ICE])
              "salacberry" -> Just (80, [FIGHTING])
              "petayaberry" -> Just (80, [POISON])
              "apicotberry" -> Just (80, [GROUND])
              "lansatberry" -> Just (80, [FLYING])
              "starfberry" -> Just (80, [PSYCHIC])
              "enigmaberry" -> Just (80, [BUG])
              "micleberry" -> Just (80, [ROCK])
              "custapberry" -> Just (80, [GHOST])
              "jabocaberry" -> Just (80, [DRAGON])
              "rowapberry" -> Just (80, [DARK])
              "roseliberry" -> Just (0, [FAIRY])
              "keeberry" -> Just (0, [FAIRY])
              "marangaberry" -> Just (0, [DARK])
              _ -> Nothing
    )

ateAbility :: T.Text -> [Type] -> [Type]
ateAbility "aerilate" [NORMAL] = [FLYING]
ateAbility "pixilate" [NORMAL] = [FAIRY]
ateAbility "refrigerate" [NORMAL] = [ICE]
ateAbility "galvanize" [NORMAL] = [ELECTRIC]
ateAbility _ t = t

liquidVoice :: EffectivePokemon -> EffectiveMove -> [Type] -> [Type]
liquidVoice EP {epAbility = ability} EM {emSound = sound} ts = if toId ability == "liquidvoice" && sound then [WATER] else ts

electrify :: Environment -> [Type] -> [Type]
electrify Env {electrified = electrified} ts = if electrified && head ts == NORMAL then [ELECTRIC] else ts

getMoveType :: EffectivePokemon -> EffectivePokemon -> Environment -> EffectiveMove -> [Type]
getMoveType attacker defender env move = (getMoveBaseType attacker defender moveName env >>> ateAbility attackerAbility >>> liquidVoice attacker move >>> getNatureType ((toId . emName) move) attacker defender env >>> electrify env) (emType move)
  where
    moveName = emName move
    attackerAbility = epAbility attacker

getNatureType :: T.Text -> EffectivePokemon -> EffectivePokemon -> Environment -> [Type] -> [Type]
getNatureType "naturalgift" atk def env _ = snd (getNaturalGiftValues atk def env)
getNatureType "naturepower" _ _ env _ = snd (getNaturePowerValues env)
getNatureType _ _ _ _ t = t

updateTmWithItem :: T.Text -> Bool -> TypeMatchup -> TypeMatchup
updateTmWithItem "airballoon" b tm@(TM am dm) = if not b then let dm' = M.insert FLYING Immune dm in TM am dm' else tm
updateTmWithItem "ironball" b tm@(TM am dm) = if not b then let dm' = M.insert GROUND Neutral dm in TM am dm' else tm
updateTmWithItem "ringtarget" b tm = if not b then removeImmunities tm else tm
updateTmWithItem _ _ tm = tm

enrichTmWithAbility :: T.Text -> T.Text -> T.Text -> TypeMatchup -> TypeMatchup
enrichTmWithAbility atkAbility defAbility moveName tm@(TM am dm)
  | atkAbility `elem` abilityIgnoringAbilities || atkAbility == "neutralizinggas" = tm
  | moveName `elem` movesThatIgnoreAbilities = tm
  | defAbility `elem` ["waterabsorb", "dryskin", "stormdrain"] = TM am (M.insert WATER Immune dm)
  | defAbility == "flashfire" = TM am (M.insert FIRE Immune dm)
  | defAbility == "levitate" = TM am (M.insert GROUND Immune dm)
  | defAbility `elem` ["motordrive", "voltabsorb", "lightningrod"] = TM am (M.insert ELECTRIC Immune dm)
  | defAbility == "sapsipper" = TM am (M.insert GRASS Immune dm)
  | defAbility == "wonderguard" = TM am wonderguardDm
  | defAbility == "scrappy" = TM am (M.insert GHOST Neutral dm)
  | otherwise = tm
  where
    wonderguardFilter StronglyEffective = Just StronglyEffective
    wonderguardFilter SuperEffective = Just SuperEffective
    wonderguardFilter x = Just Immune
    wonderguardDm' = foldl (flip $ M.update wonderguardFilter) dm (M.keys dm)
    allTypesNotPresent = allTypes L.\\ M.keys dm
    wonderguardDm = foldl (\m a -> M.insert a Immune m) wonderguardDm' allTypesNotPresent

enrichTmWithWeather :: Maybe Weather -> Bool -> Typing -> TypeMatchup -> TypeMatchup
enrichTmWithWeather (Just STRONGWIND) bool typing tm@(TM am dm) =
  let dm' = M.fromList [(ROCK, Resisted), (ICE, Resisted), (ELECTRIC, Resisted)]
   in if bool || FLYING `notElem` typing then tm else TM am (combineDefenseMap dm' dm)
enrichTmWithWeather _ _ _ tm = tm

enrichTmWithEnv :: Environment -> TypeMatchup -> TypeMatchup
enrichTmWithEnv Env {gravity = gravity} (TM am dm) = if gravity then TM am (M.insert GROUND Neutral dm) else TM am dm

thousandArrows :: T.Text -> Typing -> Int -> TypeMatchup -> TypeMatchup
thousandArrows "thousandarrows" defenderTyping tu (TM am dm) = let relation = if tu > 0 || FLYING `notElem` defenderTyping then Neutral else Resisted in TM am (M.insert GROUND relation dm)
thousandArrows _ _ _ tm = tm

getItemMultiplier :: T.Text -> EffectiveMove -> Typing -> AttackRelation -> T.Text -> (T.Text, T.Text) -> Double
getItemMultiplier "latios" _ moveType _ "souldew" _ = if hasAny moveType [DRAGON, PSYCHIC] then 1.2 else 1
getItemMultiplier "latias" _ moveType _ "souldew" _ = if hasAny moveType [DRAGON, PSYCHIC] then 1.2 else 1
getItemMultiplier "dialga" _ moveType _ "adamantorb" _ = if hasAny moveType [DRAGON, STEEL] then 1.2 else 1
getItemMultiplier "palkia" _ moveType _ "lustrousorb" _ = if hasAny moveType [DRAGON, WATER] then 1.2 else 1
getItemMultiplier "giratina" _ moveType _ "griseuosorb" _ = if hasAny moveType [DRAGON, GHOST] then 1.2 else 1
getItemMultiplier "giratinaorigin" _ moveType _ _ _ = if hasAny moveType [DRAGON, GHOST] then 1.2 else 1
getItemMultiplier _ em _ _ "metronome" _ = min 2 (1 + 0.2 * fromIntegral (emTimesUsed em))
getItemMultiplier _ _ _ _ "lifeorb" _ = 1.3
getItemMultiplier _ _ _ SuperEffective "expertbelt" _ = 1.2
getItemMultiplier _ _ moveType _ "normalgem" _ = getGemMultiplier NORMAL moveType
getItemMultiplier _ _ moveType _ "pinkbow" _ = getBowMultiplier NORMAL moveType
getItemMultiplier _ _ moveType _ "polkadotbow" _ = getBowMultiplier NORMAL moveType
getItemMultiplier _ _ moveType _ "silkscarf" _ = getItemTypeMultiplier NORMAL moveType
getItemMultiplier _ _ moveType _ "darkgem" _ = getGemMultiplier DARK moveType
getItemMultiplier _ _ moveType _ "dreadplate" _ = getPlateMultiplier DARK moveType
getItemMultiplier _ _ moveType _ "blackglasses" _ = getItemTypeMultiplier DARK moveType
getItemMultiplier _ _ moveType _ "psychicgem" _ = getGemMultiplier PSYCHIC moveType
getItemMultiplier _ _ moveType _ "twistedspoon" _ = getItemTypeMultiplier PSYCHIC moveType
getItemMultiplier _ _ moveType _ "oddincense" _ = getItemTypeMultiplier PSYCHIC moveType
getItemMultiplier _ _ moveType _ "mindplate" _ = getPlateMultiplier PSYCHIC moveType
getItemMultiplier _ _ moveType _ "ghostgem" _ = getGemMultiplier GHOST moveType
getItemMultiplier _ _ moveType _ "spelltag" _ = getItemTypeMultiplier GHOST moveType
getItemMultiplier _ _ moveType _ "spookyplate" _ = getPlateMultiplier GHOST moveType
getItemMultiplier _ _ moveType _ "flyinggem" _ = getGemMultiplier FLYING moveType
getItemMultiplier _ _ moveType _ "sharpbeak" _ = getItemTypeMultiplier FLYING moveType
getItemMultiplier _ _ moveType _ "skyplate" _ = getPlateMultiplier FLYING moveType
getItemMultiplier _ _ moveType _ "fightinggem" _ = getGemMultiplier FIGHTING moveType
getItemMultiplier _ _ moveType _ "blackbelt" _ = getItemTypeMultiplier FIGHTING moveType
getItemMultiplier _ _ moveType _ "fistplate" _ = getPlateMultiplier FIGHTING moveType
getItemMultiplier _ _ moveType _ "icegem" _ = getGemMultiplier ICE moveType
getItemMultiplier _ _ moveType _ "nevermeltice" _ = getItemTypeMultiplier ICE moveType
getItemMultiplier _ _ moveType _ "icicleplate" _ = getPlateMultiplier ICE moveType
getItemMultiplier _ _ moveType _ "steelgem" _ = getGemMultiplier STEEL moveType
getItemMultiplier _ _ moveType _ "metalcoat" _ = getItemTypeMultiplier STEEL moveType
getItemMultiplier _ _ moveType _ "ironplate" _ = getPlateMultiplier STEEL moveType
getItemMultiplier _ _ moveType _ "rockgem" _ = getGemMultiplier ROCK moveType
getItemMultiplier _ _ moveType _ "rockincense" _ = getItemTypeMultiplier ROCK moveType
getItemMultiplier _ _ moveType _ "hardstone" _ = getItemTypeMultiplier ROCK moveType
getItemMultiplier _ _ moveType _ "stoneplate" _ = getPlateMultiplier ROCK moveType
getItemMultiplier _ _ moveType _ "poisongem" _ = getGemMultiplier POISON moveType
getItemMultiplier _ _ moveType _ "poisonbarb" _ = getGemMultiplier POISON moveType
getItemMultiplier _ _ moveType _ "toxicplate" _ = getPlateMultiplier POISON moveType
getItemMultiplier _ _ moveType _ "groundgem" _ = getGemMultiplier GROUND moveType
getItemMultiplier _ _ moveType _ "softsand" _ = getItemTypeMultiplier GROUND moveType
getItemMultiplier _ _ moveType _ "earthplate" _ = getPlateMultiplier GROUND moveType
getItemMultiplier _ _ moveType _ "firegem" _ = getGemMultiplier FIRE moveType
getItemMultiplier _ _ moveType _ "charcoal" _ = getItemTypeMultiplier FIRE moveType
getItemMultiplier _ _ moveType _ "flameplate" _ = getItemTypeMultiplier FIRE moveType
getItemMultiplier _ _ moveType _ "watergem" _ = getGemMultiplier WATER moveType
getItemMultiplier _ _ moveType _ "mysticwater" _ = getItemTypeMultiplier WATER moveType
getItemMultiplier _ _ moveType _ "seaincense" _ = getItemTypeMultiplier WATER moveType
getItemMultiplier _ _ moveType _ "waveincense" _ = getItemTypeMultiplier WATER moveType
getItemMultiplier _ _ moveType _ "splashplate" _ = getPlateMultiplier WATER moveType
getItemMultiplier _ _ moveType _ "grassgem" _ = getGemMultiplier GRASS moveType
getItemMultiplier _ _ moveType _ "miracleseed" _ = getItemTypeMultiplier GRASS moveType
getItemMultiplier _ _ moveType _ "roseincense" _ = getItemTypeMultiplier GRASS moveType
getItemMultiplier _ _ moveType _ "meadowplate" _ = getPlateMultiplier GRASS moveType
getItemMultiplier _ _ moveType _ "electricgem" _ = getGemMultiplier ELECTRIC moveType
getItemMultiplier _ _ moveType _ "magnet" _ = getItemTypeMultiplier ELECTRIC moveType
getItemMultiplier _ _ moveType _ "zapplate" _ = getPlateMultiplier ELECTRIC moveType
getItemMultiplier _ _ moveType _ "fairygem" _ = getGemMultiplier FAIRY moveType
getItemMultiplier _ _ moveType _ "pixieplate" _ = getPlateMultiplier FAIRY moveType
getItemMultiplier _ _ moveType _ "buggem" _ = getGemMultiplier BUG moveType
getItemMultiplier _ _ moveType _ "silverpowder" _ = getItemTypeMultiplier BUG moveType
getItemMultiplier _ _ moveType _ "insectplate" _ = getPlateMultiplier BUG moveType
getItemMultiplier _ _ moveType _ "dragongem" _ = getGemMultiplier DRAGON moveType
getItemMultiplier _ _ moveType _ "dragonfang" _ = getItemTypeMultiplier DRAGON moveType
getItemMultiplier _ _ moveType _ "dracoplate" _ = getPlateMultiplier DRAGON moveType
getItemMultiplier _ _ moveType _ "chilanberry" _ = getBerryMultiplier NORMAL moveType
getItemMultiplier _ _ moveType SuperEffective "babiriberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier STEEL moveType else 1
getItemMultiplier _ _ moveType SuperEffective "chartiberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ROCK moveType else 1
getItemMultiplier _ _ moveType SuperEffective "chopleberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FIGHTING moveType else 1
getItemMultiplier _ _ moveType SuperEffective "cobaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FLYING moveType else 1
getItemMultiplier _ _ moveType SuperEffective "colburberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier DARK moveType else 1
getItemMultiplier _ _ moveType SuperEffective "habanberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier DRAGON moveType else 1
getItemMultiplier _ _ moveType SuperEffective "kasibberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GHOST moveType else 1
getItemMultiplier _ _ moveType SuperEffective "kebiaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier POISON moveType else 1
getItemMultiplier _ _ moveType SuperEffective "occaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FIRE moveType else 1
getItemMultiplier _ _ moveType SuperEffective "passhoberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier WATER moveType else 1
getItemMultiplier _ _ moveType SuperEffective "payapaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier PSYCHIC moveType else 1
getItemMultiplier _ _ moveType SuperEffective "rindoberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GRASS moveType else 1
getItemMultiplier _ _ moveType SuperEffective "shucaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GROUND moveType else 1
getItemMultiplier _ _ moveType SuperEffective "tangaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier BUG moveType else 1
getItemMultiplier _ _ moveType SuperEffective "wacanberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ELECTRIC moveType else 1
getItemMultiplier _ _ moveType SuperEffective "yacheberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ICE moveType else 1
getItemMultiplier _ _ _ _ _ _ = 1

getGeneralTypeMultiplier :: (Foldable t, Eq a, Num p) => p -> a -> t a -> p
getGeneralTypeMultiplier mult tipe moveType = if tipe `elem` moveType then mult else 1

getItemTypeMultiplier :: Type -> [Type] -> Double
getItemTypeMultiplier = getGeneralTypeMultiplier 1.2

getGemMultiplier :: Type -> [Type] -> Double
getGemMultiplier = getGeneralTypeMultiplier 1.3

getBowMultiplier :: Type -> [Type] -> Double
getBowMultiplier = getGeneralTypeMultiplier 1.2

getPlateMultiplier :: Type -> [Type] -> Double
getPlateMultiplier = getGeneralTypeMultiplier 1.2

getBerryMultiplier :: Type -> [Type] -> Double
getBerryMultiplier = getGeneralTypeMultiplier 0.5

getScreenMultiplier :: [Screen] -> T.Text -> AttackType -> Double
getScreenMultiplier [] _ _ = 1
getScreenMultiplier (REFLECT : ss) a t = if t == PHYSICAL && a /= "infiltrator" then 0.5 else getScreenMultiplier ss a t
getScreenMultiplier (LIGHT_SCREEN : ss) a t = if t == SPECIAL && a /= "infiltrator" then 0.5 else getScreenMultiplier ss a t
getScreenMultiplier (AURORA_VEIL : ss) a t = if a /= "infiltrator" then 0.5 else 1

getWeatherMult :: [Type] -> Weather -> Double
getWeatherMult mType RAIN
  | hasAny mType [WATER] = 1.5
  | hasAny mType [FIRE] = 0.5
  | otherwise = 1
getWeatherMult mType HEAVYRAIN
  | hasAny mType [WATER] = 1.5
  | hasAny mType [FIRE] = 0
  | otherwise = 1
getWeatherMult mType SUN
  | hasAny mType [FIRE] = 1.5
  | hasAny mType [WATER] = 0.5
  | otherwise = 1
getWeatherMult mType HEAVYSUN
  | hasAny mType [FIRE] = 1.5
  | hasAny mType [WATER] = 0
  | otherwise = 1
getWeatherMult _ _ = 1

isGrounded :: EffectivePokemon -> Bool
isGrounded EP {..} = grounded
  where
    ts = epTyping
    ability = toId epAbility
    item = fromMaybe "" (epItem <&> toId . iName)
    grounded = FLYING `elem` ts || ability == "levitate" || item == "airballoon" || epRisen

willCrit :: EffectivePokemon -> EffectivePokemon -> Bool
willCrit EP {epAbility = "merciless"} EP {epStatus = Just x} = True
willCrit _ _ = False

isBurned :: EffectivePokemon -> Bool
isBurned EP {epStatus = Just BURN} = True
isBurned _ = False

hasItem :: T.Text -> EffectivePokemon -> Bool
hasItem name EP {epItem = Just x} = iName x == name
hasItem _ _ = False

canUseItem :: EffectivePokemon -> Environment -> Bool
canUseItem _ Env {magicRoom = True} = False
canUseItem EP {epAbility = "klutz"} _ = False
canUseItem _ _ = True

canUseBerry :: EffectivePokemon -> EffectivePokemon -> Environment -> Bool
canUseBerry berryUser def env = canUseItem berryUser env && epAbility def /= "unnerve"
