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

getAttackAndMult :: T.Text -> AttackType -> (EffectiveStats, Multipliers) -> (EffectiveStats, Multipliers) -> (Double, Double) -> (Int, StatMultiplier, Double, String)
getAttackAndMult "bodypress" _ (es, em) _ (atkMult, spaMult) = (defStat es, defM em, atkMult, "Def")
getAttackAndMult "foulplay" _ _ (es, em) (atkMult, spaMult) = (atkStat es, atkM em, atkMult, "Atk")
getAttackAndMult "naturepower" _ (es, em) _ (atkMult, spaMult) = (spaStat es, spaM em, spaMult, "SpA")
getAttackAndMult "naturalgift" _ (es, em) _ (atkMult, spaMult) = (spaStat es, spaM em, spaMult, "SpA")
getAttackAndMult move SPECIAL (es, em) _ (atkMult, spaMult) = (spaStat es, spaM em, spaMult, "SpA")
getAttackAndMult move PHYSICAL (es, em) _ (atkMult, spaMult) = (atkStat es, atkM em, atkMult, "Atk")
getAttackAndMult move OTHER (es, em) _ (atkMult, spaMult) = (0, 0, 0, "")

getDefenseAndMult :: T.Text -> AttackType -> (EffectiveStats, Multipliers) -> (EffectiveStats, Multipliers) -> (Double, Double) -> (Int, StatMultiplier, Double, String)
getDefenseAndMult "psystrike" _ _ (es, em) (defMult, spdMult) = (defStat es, defM em, defMult, "Def")
getDefenseAndMult "psyshock" _ _ (es, em) (defMult, spdMult) = (defStat es, defM em, defMult, "Def")
getDefenseAndMult "secretsword" _ _ (es, em) (defMult, spdMult) = (defStat es, defM em, defMult, "Def")
getDefenseAndMult "naturepower" _ _ (es, em) (defMult, spdMult) = (spdStat es, spdM em, spdMult, "SpD")
getDefenseAndMult "naturalgift" _ _ (es, em) (defMult, spdMult) = (spdStat es, spdM em, spdMult, "SpD")
getDefenseAndMult "sacredsword" _ _ (es, _) (defMult, spdMult) = (defStat es, 0, defMult, "Def")
getDefenseAndMult "darkestlariat" _ _ (es, _) (defMult, spdMult) = (defStat es, 0, defMult, "Def")
getDefenseAndMult "chipaway" _ _ (es, _) (defMult, spdMult) = (defStat es, 0, defMult, "Def")
getDefenseAndMult "wickedblow" _ _ (es, _) (defMult, spdMult) = (defStat es, 0, defMult, "Def")
getDefenseAndMult "surgingstrikes" _ _ (es, _) (defMult, spdMult) = (defStat es, 0, defMult, "Def")
getDefenseAndMult _ PHYSICAL _ (es, em) (defMult, spdMult) = (defStat es, defM em, defMult, "Def")
getDefenseAndMult _ SPECIAL _ (es, em) (defMult, spdMult) = (spdStat es, spdM em, spdMult, "SpD")
getDefenseAndMult _ OTHER _ (es, em) (defMult, spdMult) = (defStat es, defM em, defMult, "")

applyWonderRoom :: Environment -> EffectiveStats -> EffectiveStats
applyWonderRoom Env {wonderRoom = True} ES {..} = ES hpStat atkStat spdStat spaStat defStat speStat
applyWonderRoom _ es = es

getMultipliers :: EffectivePokemon -> Multipliers -> Multipliers
getMultipliers EP {epAbilityId = "unaware"} sm = Multipliers 0 0 0 0 0
getMultipliers _ sm = sm

getMoveMultiplier :: EffectivePokemon -> EffectivePokemon -> EffectiveMove -> Environment -> Double
getMoveMultiplier attacker defender em@EM {emId = "fishiousrend"} env = let moveOrder = getMoveOrder attacker defender em env in if moveOrder == FIRST then 2 else 1
getMoveMultiplier attacker defender em@EM {emId = "boltbeak"} env = let moveOrder = getMoveOrder attacker defender em env in if moveOrder == FIRST then 2 else 1
getMoveMultiplier _ _ em@EM {emId = "avalanche"} env = if hitBefore env then 2 else 1
getMoveMultiplier _ _ em@EM {emId = "assurance"} env = if hitBefore env then 2 else 1
getMoveMultiplier _ EP {epHPPercentage = rem} EM {emId = "brine"} env = if rem < 50 then 2 else 1
getMoveMultiplier _ _ EM {emId = "pursuit"} Env {switchingOut = True} = 2
getMoveMultiplier EP {epItem = Nothing} _ EM {emId = "acrobatics"} _ = 2
getMoveMultiplier EP {epStatsLowered = True} _ EM {emId = "lashout"} _ = 2
getMoveMultiplier _ _ EM {emId = "mistyexplosion"} Env {activeTerrain = Just MISTY} = 1.5
getMoveMultiplier _ defender EM {emId = "risingvoltage"} Env {activeTerrain = Just ELECTRIC_T} = if isGrounded defender then 2 else 1
getMoveMultiplier attacker _ EM {emId = "expandingforce"} Env {activeTerrain = Just PSYCHIC_T} = if isGrounded attacker then 1.5 else 1
getMoveMultiplier _ EP {epStatus = Just PARALYZED} EM {emId = "smellingsalts"} _ = 2
getMoveMultiplier _ EP {epStatus = Just x} EM {emId = "hex"} _ = 2
getMoveMultiplier _ EP {epStatus = Just POISONED} EM {emId = "venoshock"} _ = 2
getMoveMultiplier _ EP {epStatus = Just SLEEP} EM {emId = "wakeupslap"} _ = 2
getMoveMultiplier _ _ EM {emId = "terrainpulse"} Env {activeTerrain = Just x} = 2
getMoveMultiplier _ _ EM {emId = "weatherball"} Env {activeWeather = Just STRONGWIND} = 1
getMoveMultiplier _ _ EM {emId = "weatherball"} Env {activeWeather = Just x} = 2
getMoveMultiplier _ EP {epItem = Just x} EM {emId = "knockoff"} _ = if iId x `notElem` unknockableItems then 1.5 else 1
getMoveMultiplier EP {epStatus = Just x} _ EM {emId = "facade"} _ = 2
getMoveMultiplier attacker defender move env = 1

getTerrainMultiplier :: Environment -> [Type] -> Double
getTerrainMultiplier Env {activeTerrain = Just GRASSY} tipe = if GRASS `elem` tipe then 1.3 else 1
getTerrainMultiplier Env {activeTerrain = Just ELECTRIC_T} tipe = if ELECTRIC `elem` tipe then 1.3 else 1
getTerrainMultiplier Env {activeTerrain = Just PSYCHIC_T} tipe = if PSYCHIC `elem` tipe then 1.3 else 1
getTerrainMultiplier Env {activeTerrain = Just MISTY} tipe = if DRAGON `elem` tipe then 0.5 else 1
getTerrainMultiplier _ _ = 1

-- | AAMM == AttackAbilityMove
getAbilityMultiplier :: T.Text -> EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Double
getAbilityMultiplier _ _ _ EP {epAbilityId = "neutralizinggas"} _ = 1
getAbilityMultiplier "aerilate" EM {emId = moveName, emType = mType} _ _ _ = if moveName `notElem` ["naturalgift", "naturepower"] && NORMAL == mType then 1.2 else 1
getAbilityMultiplier "galvanize" EM {emId = moveName, emType = mType} _ _ _ = if moveName `notElem` ["naturalgift", "naturepower"] && NORMAL == mType then 1.2 else 1
getAbilityMultiplier "pixilate" EM {emId = moveName, emType = mType} _ _ _ = if moveName `notElem` ["naturalgift", "naturepower"] && NORMAL == mType then 1.2 else 1
getAbilityMultiplier "refrigerate" EM {emId = moveName, emType = mType} _ _ _ = if moveName `notElem` ["naturalgift", "naturepower"] && NORMAL == mType then 1.2 else 1
getAbilityMultiplier "sheerforce" EM {emHasSecondary = True} _ _ _ = 1.3
getAbilityMultiplier "technician" EM {emBp = bp} _ _ _ = if bp <= 60 then 1.5 else 1
getAbilityMultiplier "analytic" em attacker defender env =
  let order = getMoveOrder attacker defender em env
      moveName = emId em
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
getAbilityMultiplier _ EM {emType = tipe} _ EP {epAbilityId = "thickfat"} _ = if tipe `elem` [ICE, FIRE] then 0.5 else 1
getAbilityMultiplier "torrent" EM {emType = tipe} EP {epHPPercentage = percentage} _ _ = if tipe == WATER && percentage / 100 < 1 / 3 then 1.5 else 1
getAbilityMultiplier "blaze" EM {emType = tipe} EP {epHPPercentage = percentage} _ _ = if tipe == FIRE && percentage / 100 < 1 / 3 then 1.5 else 1
getAbilityMultiplier "overgrow" EM {emType = tipe} EP {epHPPercentage = percentage} _ _ = if tipe == GRASS && percentage / 100 < 1 / 3 then 1.5 else 1
getAbilityMultiplier "dragonsmaw" EM {emType = tipe} _ _ _ = if tipe == DRAGON then 1.5 else 1
getAbilityMultiplier "transistor" EM {emType = tipe} _ _ _ = if tipe == ELECTRIC then 1.5 else 1
getAbilityMultiplier "waterbubble" EM {emType = tipe} _ _ _ = if tipe == WATER then 2 else 1
getAbilityMultiplier "megalauncher" EM {emPulse = True} _ _ _ = 1.5
getAbilityMultiplier "stakeout" _ _ _ Env {switchingOut = True} = 2
getAbilityMultiplier "toughclaws" EM {isContact = True} _ _ _ = 1.3
getAbilityMultiplier ability move attacker defender environment = 1

getRivalryMultiplier :: EffectivePokemon -> EffectivePokemon -> Double
getRivalryMultiplier EP {epGender = attackerGender, epAbilityId = "rivalry"} EP {epGender = defenderGender}
  | (attackerGender == MALE && defenderGender == MALE) || (attackerGender == FEMALE && defenderGender == FEMALE) = 1.25
  | (attackerGender == MALE && defenderGender == FEMALE) || (attackerGender == FEMALE && defenderGender == MALE) = 0.75
  | otherwise = 1
getRivalryMultiplier _ _ = 1

getAttackStatMultiplier :: T.Text -> EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Double
getAttackStatMultiplier "hugepower" _ _ _ _ = 2
getAttackStatMultiplier "purepower" _ _ _ _ = 2
getAttackStatMultiplier "hustle" _ _ _ _ = 1.5
getAttackStatMultiplier "flowergift" _ _ _ Env {activeWeather = Just x} = if x `elem` [SUN, HEAVYSUN] then 1.5 else 1
getAttackStatMultiplier "gorillatactics" _ _ _ _ = 1.5
getAttackStatMultiplier "guts" _ EP {epStatus = Just x} _ _ = 1.5
getAttackStatMultiplier "toxicboost" _ EP {epStatus = Just POISONED} _ _ = 1.5
getAttackStatMultiplier "toxicboost" _ EP {epStatus = Just BADLY_POISONED} _ _ = 1.5
getAttackStatMultiplier _ _ _ _ _ = 1

getDefenseStatMultiplier :: T.Text -> EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Double
getDefenseStatMultiplier "furcoat" _ _ _ _ = 2
getDefenseStatMultiplier "grasspelt" _ _ _ Env {activeTerrain = Just GRASSY} = 1.5
getDefenseStatMultiplier "marvelscale" _ _ EP {epStatus = Just x} _ = 1.5
getDefenseStatMultiplier _ _ _ _ _ = 1

getSpecialStatMultiplier :: T.Text -> EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Double
getSpecialStatMultiplier "flareboost" _ EP {epStatus = Just BURN} _ _ = 1.5
getSpecialStatMultiplier "solarpower" _ _ _ Env {activeWeather = Just weather} = if weather `elem` [SUN, HEAVYSUN] then 1.5 else 1
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
getEMCategory EP {epLevel = level} EM {emId = "shellsidearm"} atkStats atkMults defStats defMults =
  let attackStat = getTotalStat (atkStat atkStats) (atkM atkMults)
      specialAttackStat = getTotalStat (spaStat atkStats) (spaM atkMults)
      defenseStat = getTotalStat (defStat defStats) (defM defMults)
      specialDefenseStat = getTotalStat (spdStat defStats) (spdM defMults)
   in getShellSideArmCat level attackStat specialAttackStat defenseStat specialDefenseStat
getEMCategory _ EM {emId = "photongeyser"} atkStats atkMults _ _ =
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
      | epAbilityId attacker == "stall" = -6
      | epAbilityId attacker == "triage" && emDrain move = 3
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
    speedMultiplier = getSpeedMultiplier epAbilityId ep env
    choiceScarf = if hasItem "choicescarf" ep then 1.5 else 1
    boostMultiplier = (getMultiplier . speM) epMultiplier
    totalMultiplier = speedMultiplier * boostMultiplier * choiceScarf
    natureEffect = getNatureEffect SPE epNature
    speedStat = calcStat epLevel (speIv epIvs) (speEv epEvs) natureEffect (findBaseStat epStats SPE)

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
getBp m = getBp' (toId . emId $ m) m

getBp' :: T.Text -> EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Environment -> Int
getBp' "heavyslam" _ atk def _ = getWeightRatioPower atk def
getBp' "heatcrash" _ atk def _ = getWeightRatioPower atk def
getBp' "lowkick" _ _ def _ = getWeightPower (getWeight def)
getBp' "grassknot" _ _ def _ = getWeightPower (getWeight def)
getBp' "skydrop" EM {emBp = bp} _ def _ = if getWeight def >= 200 then 0 else bp
getBp' "gyroball" _ atk def env =
  let attackerSpeed = fromIntegral $ getSpeedAttacker atk env :: Double
      defenderSpeed = fromIntegral $ getSpeedDefender def env :: Double
   in min 150 (floor ((25 * defenderSpeed / attackerSpeed) + 1))
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
getWeight EP {epWeight = baseWeight, epAbilityId = ability}
  | toId ability == "heavymetal" = 2 * baseWeight
  | toId ability == "lightmetal" = fromIntegral baseWeight *// 0.5
  | otherwise = baseWeight

getRatioPower :: Int -> Int
getRatioPower ratio
  | ratio >= 5 = 120
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
getDefensiveAbilityMultiplier "damp" _ EM {emId = name} _ _ _ = if toId name `elem` ["selfdestruct", "explosion", "mindblown", "mistyexplosion"] then 0 else 1
getDefensiveAbilityMultiplier "dazzling" _ EM {emPriority = prio} _ _ _ = if prio > 0 then 0 else 1
getDefensiveAbilityMultiplier "dryskin" _ _ typing _ _ = if FIRE `elem` typing then 1.25 else 1
getDefensiveAbilityMultiplier "queenlymajesty" _ EM {emPriority = prio} _ _ _ = if prio > 0 then 0 else 1
getDefensiveAbilityMultiplier ab ep em typing cat super = 1

getAttackAbilityMultiplier :: T.Text -> EffectivePokemon -> EffectivePokemon -> EffectiveMove -> Environment -> [Type] -> AttackType -> AttackRelation -> Double
getAttackAbilityMultiplier "neuroforce" _ _ _ _ _ _ SuperEffective = 1.25
getAttackAbilityMultiplier "neuroforce" _ _ _ _ _ _ StronglyEffective = 1.25
getAttackAbilityMultiplier ab attacker defender em env typing cat effectiveness = 1

getPokemonType :: EffectivePokemon -> [Type]
getPokemonType ep@EP {epId = "silvally"} = fromMaybe (epTyping ep) (epItem ep >>= iOnMemory >>= (fmap (: []) . readMaybe . T.unpack))
getPokemonType p@EP {epId = "arceus"} = fromMaybe (epTyping p) (epItem p >>= iOnPlate >>= (fmap (: []) . readMaybe . T.unpack))
getPokemonType ep = epTyping ep

getMoveBaseType :: EffectivePokemon -> EffectivePokemon -> T.Text -> Environment -> Type -> [Type]
getMoveBaseType _ _ "flyingpress" _ t = [t, FLYING]
getMoveBaseType p _ "multiattack" Env {magicRoom = isRoom} t =
  if not $ "silvally" `T.isInfixOf` epId p || epAbilityId p == "klutz" || isRoom
    then [t]
    else fromMaybe [t] (epItem p >>= iOnMemory >>= (fmap (: []) . readMaybe . T.unpack))
getMoveBaseType p _ "judgment" Env {magicRoom = isRoom} t =
  if not $ "arceus" `T.isInfixOf` epId p || epAbilityId p == "klutz" || isRoom
    then [t]
    else fromMaybe [t] (epItem p >>= iOnPlate >>= (fmap (: []) . readMaybe . T.unpack))
getMoveBaseType p _ "technoblast" Env {magicRoom = isRoom} t =
  if not $ "genesect" `T.isInfixOf` epId p || epAbilityId p == "klutz" || isRoom
    then [t]
    else fromMaybe [t] (epItem p >>= iOnDrive >>= (fmap (: []) . readMaybe . T.unpack))
getMoveBaseType EP {epTyping = typing} _ "revelationdance" _ _ = [head typing]
getMoveBaseType EP {epAbilityId = "normalize"} _ _ _ _ = [NORMAL]
getMoveBaseType _ _ "terrainpulse" Env {activeTerrain = Just x} _ = case x of
  ELECTRIC_T -> [ELECTRIC]
  PSYCHIC_T -> [PSYCHIC]
  MISTY -> [FAIRY]
  GRASSY -> [GRASS]
getMoveBaseType _ _ "weatherball" Env {activeWeather = Just weather} _ = case weather of
  SANDSTORM -> [ROCK]
  HAIL -> [ICE]
  RAIN -> [WATER]
  SUN -> [FIRE]
  HEAVYRAIN -> [WATER]
  HEAVYSUN -> [FIRE]
  STRONGWIND -> [NORMAL]
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
getNaturalGiftValues EP {epItem = item, epAbilityId = ability} EP {epAbilityId = a'} Env {magicRoom = isRoom} =
  fromMaybe
    (0, [NORMAL])
    ( item
        >>= \i ->
          if (not . iBerry) i || toId ability == "klutz" || isRoom || toId a' == "unnerve"
            then Just (0, [NORMAL])
            else case iId i of
              "cheriberry" -> Just (80, [FIRE])
              "chestoberry" -> Just (80, [WATER])
              "pechaberry" -> Just (80, [ELECTRIC])
              "rawstberry" -> Just (80, [GRASS])
              "aspearberry" -> Just (80, [ICE])
              "leppaberry" -> Just (80, [FIGHTING])
              "oranberry" -> Just (80, [POISON])
              "persimberry" -> Just (80, [GROUND])
              "lumberry" -> Just (80, [FLYING])
              "sitrusberry" -> Just (80, [PSYCHIC])
              "figyberry" -> Just (80, [BUG])
              "wikiberry" -> Just (80, [ROCK])
              "magoberry" -> Just (80, [GHOST])
              "aguavberry" -> Just (80, [DRAGON])
              "iapapaberry" -> Just (80, [DARK])
              "razzberry" -> Just (80, [STEEL])
              "blukberry" -> Just (90, [FIRE])
              "nanabberry" -> Just (90, [WATER])
              "wepearberry" -> Just (90, [ELECTRIC])
              "pinapberry" -> Just (90, [GRASS])
              "pomegberry" -> Just (90, [ICE])
              "kelpsyberry" -> Just (90, [FIGHTING])
              "qualotberry" -> Just (90, [POISON])
              "hondewberry" -> Just (90, [GROUND])
              "grepaberry" -> Just (90, [FLYING])
              "tamatoberry" -> Just (90, [PSYCHIC])
              "cornnberry" -> Just (90, [BUG])
              "magostberry" -> Just (90, [ROCK])
              "rabutaberry" -> Just (90, [GHOST])
              "nomelberry" -> Just (90, [DRAGON])
              "spelonberry" -> Just (90, [DARK])
              "pamtreberry" -> Just (90, [STEEL])
              "watmelberry" -> Just (100, [FIRE])
              "durinberry" -> Just (100, [WATER])
              "belueberry" -> Just (100, [ELECTRIC])
              "occaberry" -> Just (80, [FIRE])
              "passhoberry" -> Just (80, [WATER])
              "wacanberry" -> Just (80, [ELECTRIC])
              "rindoberry" -> Just (80, [GRASS])
              "yacheberry" -> Just (80, [ICE])
              "chopleberry" -> Just (80, [FIGHTING])
              "kebiaberry" -> Just (80, [POISON])
              "shucaberry" -> Just (80, [GROUND])
              "cobaberry" -> Just (80, [FLYING])
              "payapaberry" -> Just (80, [PSYCHIC])
              "tangaberry" -> Just (80, [BUG])
              "chartiberry" -> Just (80, [ROCK])
              "kasibberry" -> Just (80, [GHOST])
              "habanberry" -> Just (80, [DRAGON])
              "colburberry" -> Just (80, [DARK])
              "babiriberry" -> Just (80, [STEEL])
              "chilanberry" -> Just (80, [NORMAL])
              "liechiberry" -> Just (100, [GRASS])
              "ganlonberry" -> Just (100, [ICE])
              "salacberry" -> Just (100, [FIGHTING])
              "petayaberry" -> Just (100, [POISON])
              "apicotberry" -> Just (100, [GROUND])
              "lansatberry" -> Just (100, [FLYING])
              "starfberry" -> Just (100, [PSYCHIC])
              "enigmaberry" -> Just (100, [BUG])
              "micleberry" -> Just (100, [ROCK])
              "custapberry" -> Just (100, [GHOST])
              "jabocaberry" -> Just (100, [DRAGON])
              "rowapberry" -> Just (100, [DARK])
              "roseliberry" -> Just (80, [FAIRY])
              "keeberry" -> Just (100, [FAIRY])
              "marangaberry" -> Just (100, [DARK])
              _ -> Nothing
    )

ateAbility :: T.Text -> [Type] -> [Type]
ateAbility "aerilate" [NORMAL] = [FLYING]
ateAbility "pixilate" [NORMAL] = [FAIRY]
ateAbility "refrigerate" [NORMAL] = [ICE]
ateAbility "galvanize" [NORMAL] = [ELECTRIC]
ateAbility _ t = t

liquidVoice :: EffectivePokemon -> EffectiveMove -> [Type] -> [Type]
liquidVoice EP {epAbilityId = ability} EM {emSound = sound} ts = if ability == "liquidvoice" && sound then [WATER] else ts

electrify :: Environment -> [Type] -> [Type]
electrify Env {electrified = electrified} ts = if electrified && head ts == NORMAL then [ELECTRIC] else ts

getMoveType :: EffectivePokemon -> EffectivePokemon -> Environment -> EffectiveMove -> [Type]
getMoveType attacker defender env move = (getMoveBaseType attacker defender moveName env >>> ateAbility attackerAbility >>> liquidVoice attacker move >>> getNatureType ((toId . emId) move) attacker defender env >>> electrify env) (emType move)
  where
    moveName = emId move
    attackerAbility = epAbilityId attacker

getNatureType :: T.Text -> EffectivePokemon -> EffectivePokemon -> Environment -> [Type] -> [Type]
getNatureType "naturalgift" atk def env _ = snd (getNaturalGiftValues atk def env)
getNatureType "naturepower" _ _ env _ = snd (getNaturePowerValues env)
getNatureType _ _ _ _ t = t

updateTmWithItem :: T.Text -> Bool -> TypeMatchup -> TypeMatchup
updateTmWithItem "airballoon" b tm@(TM t am dm) = if not b then let dm' = M.insert FLYING Immune dm in TM t am dm' else tm
updateTmWithItem "ironball" b tm@(TM t am dm) = if not b then let dm' = M.insert GROUND Neutral dm in TM t am dm' else tm
updateTmWithItem "ringtarget" b tm = if not b then removeImmunities tm else tm
updateTmWithItem _ _ tm = tm

enrichTmWithAbility :: T.Text -> T.Text -> T.Text -> TypeMatchup -> TypeMatchup
enrichTmWithAbility atkAbility defAbility moveName tm@(TM t am dm)
  | atkAbility `elem` abilityIgnoringAbilities || atkAbility == "neutralizinggas" = tm
  | moveName `elem` movesThatIgnoreAbilities = tm
  | defAbility `elem` ["waterabsorb", "dryskin", "stormdrain"] = TM t am (M.insert WATER Immune dm)
  | defAbility == "flashfire" = TM t am (M.insert FIRE Immune dm)
  | defAbility == "levitate" = TM t am (M.insert GROUND Immune dm)
  | defAbility `elem` ["motordrive", "voltabsorb", "lightningrod"] = TM t am (M.insert ELECTRIC Immune dm)
  | defAbility == "sapsipper" = TM t am (M.insert GRASS Immune dm)
  | defAbility == "wonderguard" = TM t am wonderguardDm
  | atkAbility == "scrappy" = if GHOST `elem` t then TM t am ((M.insert FIGHTING Neutral . M.insert NORMAL Neutral) dm) else tm
  | otherwise = tm
  where
    wonderguardFilter StronglyEffective = Just StronglyEffective
    wonderguardFilter SuperEffective = Just SuperEffective
    wonderguardFilter x = Just Immune
    wonderguardDm' = foldl (flip $ M.update wonderguardFilter) dm (M.keys dm)
    allTypesNotPresent = allTypes L.\\ M.keys dm
    wonderguardDm = foldl (\m a -> M.insert a Immune m) wonderguardDm' allTypesNotPresent

enrichTmWithWeather :: Maybe Weather -> Bool -> Typing -> TypeMatchup -> TypeMatchup
enrichTmWithWeather (Just STRONGWIND) bool typing tm@(TM t am dm) =
  let dm' = M.fromList [(ROCK, Resisted), (ICE, Resisted), (ELECTRIC, Resisted)]
   in if bool || FLYING `notElem` t then tm else TM t am (combineDefenseMap dm' dm)
enrichTmWithWeather _ _ _ tm = tm

enrichTmWithEnv :: Environment -> TypeMatchup -> TypeMatchup
enrichTmWithEnv Env {gravity = gravity} (TM t am dm) = if gravity then TM t am (M.insertWith (\mu cu -> if cu == Immune then Neutral else cu) GROUND Neutral dm) else TM t am dm

thousandArrows :: T.Text -> Typing -> Int -> TypeMatchup -> TypeMatchup
thousandArrows "thousandarrows" defenderTyping tu (TM t am dm) = let relation = if tu > 0 || FLYING `notElem` defenderTyping then Neutral else Resisted in TM t am (M.insertWith (\r cr -> if cr == Immune then r else cr) GROUND relation dm)
thousandArrows _ _ _ tm = tm

getOffenseItemMultiplier :: T.Text -> EffectiveMove -> Typing -> AttackRelation -> T.Text -> Double
getOffenseItemMultiplier "latios" _ moveType _ "souldew" = if hasAny moveType [DRAGON, PSYCHIC] then 1.2 else 1
getOffenseItemMultiplier "latias" _ moveType _ "souldew" = if hasAny moveType [DRAGON, PSYCHIC] then 1.2 else 1
getOffenseItemMultiplier "dialga" _ moveType _ "adamantorb" = if hasAny moveType [DRAGON, STEEL] then 1.2 else 1
getOffenseItemMultiplier "palkia" _ moveType _ "lustrousorb" = if hasAny moveType [DRAGON, WATER] then 1.2 else 1
getOffenseItemMultiplier "giratina" _ moveType _ "griseuosorb" = if hasAny moveType [DRAGON, GHOST] then 1.2 else 1
getOffenseItemMultiplier "giratinaorigin" _ moveType _ _ = if hasAny moveType [DRAGON, GHOST] then 1.2 else 1
getOffenseItemMultiplier _ em _ _ "metronome" = min 2 (1 + 0.2 * fromIntegral (emTimesUsed em))
getOffenseItemMultiplier _ _ _ _ "lifeorb" = 1.3
getOffenseItemMultiplier _ _ _ SuperEffective "expertbelt" = 1.2
getOffenseItemMultiplier _ _ _ StronglyEffective "expertbelt" = 1.2
getOffenseItemMultiplier _ _ moveType _ "normalgem" = getGemMultiplier NORMAL moveType
getOffenseItemMultiplier _ _ moveType _ "pinkbow" = getBowMultiplier NORMAL moveType
getOffenseItemMultiplier _ _ moveType _ "polkadotbow" = getBowMultiplier NORMAL moveType
getOffenseItemMultiplier _ _ moveType _ "silkscarf" = getItemTypeMultiplier NORMAL moveType
getOffenseItemMultiplier _ _ moveType _ "darkgem" = getGemMultiplier DARK moveType
getOffenseItemMultiplier _ _ moveType _ "dreadplate" = getPlateMultiplier DARK moveType
getOffenseItemMultiplier _ _ moveType _ "blackglasses" = getItemTypeMultiplier DARK moveType
getOffenseItemMultiplier _ _ moveType _ "psychicgem" = getGemMultiplier PSYCHIC moveType
getOffenseItemMultiplier _ _ moveType _ "twistedspoon" = getItemTypeMultiplier PSYCHIC moveType
getOffenseItemMultiplier _ _ moveType _ "oddincense" = getItemTypeMultiplier PSYCHIC moveType
getOffenseItemMultiplier _ _ moveType _ "mindplate" = getPlateMultiplier PSYCHIC moveType
getOffenseItemMultiplier _ _ moveType _ "ghostgem" = getGemMultiplier GHOST moveType
getOffenseItemMultiplier _ _ moveType _ "spelltag" = getItemTypeMultiplier GHOST moveType
getOffenseItemMultiplier _ _ moveType _ "spookyplate" = getPlateMultiplier GHOST moveType
getOffenseItemMultiplier _ _ moveType _ "flyinggem" = getGemMultiplier FLYING moveType
getOffenseItemMultiplier _ _ moveType _ "sharpbeak" = getItemTypeMultiplier FLYING moveType
getOffenseItemMultiplier _ _ moveType _ "skyplate" = getPlateMultiplier FLYING moveType
getOffenseItemMultiplier _ _ moveType _ "fightinggem" = getGemMultiplier FIGHTING moveType
getOffenseItemMultiplier _ _ moveType _ "blackbelt" = getItemTypeMultiplier FIGHTING moveType
getOffenseItemMultiplier _ _ moveType _ "fistplate" = getPlateMultiplier FIGHTING moveType
getOffenseItemMultiplier _ _ moveType _ "icegem" = getGemMultiplier ICE moveType
getOffenseItemMultiplier _ _ moveType _ "nevermeltice" = getItemTypeMultiplier ICE moveType
getOffenseItemMultiplier _ _ moveType _ "icicleplate" = getPlateMultiplier ICE moveType
getOffenseItemMultiplier _ _ moveType _ "steelgem" = getGemMultiplier STEEL moveType
getOffenseItemMultiplier _ _ moveType _ "metalcoat" = getItemTypeMultiplier STEEL moveType
getOffenseItemMultiplier _ _ moveType _ "ironplate" = getPlateMultiplier STEEL moveType
getOffenseItemMultiplier _ _ moveType _ "rockgem" = getGemMultiplier ROCK moveType
getOffenseItemMultiplier _ _ moveType _ "rockincense" = getItemTypeMultiplier ROCK moveType
getOffenseItemMultiplier _ _ moveType _ "hardstone" = getItemTypeMultiplier ROCK moveType
getOffenseItemMultiplier _ _ moveType _ "stoneplate" = getPlateMultiplier ROCK moveType
getOffenseItemMultiplier _ _ moveType _ "poisongem" = getGemMultiplier POISON moveType
getOffenseItemMultiplier _ _ moveType _ "poisonbarb" = getGemMultiplier POISON moveType
getOffenseItemMultiplier _ _ moveType _ "toxicplate" = getPlateMultiplier POISON moveType
getOffenseItemMultiplier _ _ moveType _ "groundgem" = getGemMultiplier GROUND moveType
getOffenseItemMultiplier _ _ moveType _ "softsand" = getItemTypeMultiplier GROUND moveType
getOffenseItemMultiplier _ _ moveType _ "earthplate" = getPlateMultiplier GROUND moveType
getOffenseItemMultiplier _ _ moveType _ "firegem" = getGemMultiplier FIRE moveType
getOffenseItemMultiplier _ _ moveType _ "charcoal" = getItemTypeMultiplier FIRE moveType
getOffenseItemMultiplier _ _ moveType _ "flameplate" = getItemTypeMultiplier FIRE moveType
getOffenseItemMultiplier _ _ moveType _ "watergem" = getGemMultiplier WATER moveType
getOffenseItemMultiplier _ _ moveType _ "mysticwater" = getItemTypeMultiplier WATER moveType
getOffenseItemMultiplier _ _ moveType _ "seaincense" = getItemTypeMultiplier WATER moveType
getOffenseItemMultiplier _ _ moveType _ "waveincense" = getItemTypeMultiplier WATER moveType
getOffenseItemMultiplier _ _ moveType _ "splashplate" = getPlateMultiplier WATER moveType
getOffenseItemMultiplier _ _ moveType _ "grassgem" = getGemMultiplier GRASS moveType
getOffenseItemMultiplier _ _ moveType _ "miracleseed" = getItemTypeMultiplier GRASS moveType
getOffenseItemMultiplier _ _ moveType _ "roseincense" = getItemTypeMultiplier GRASS moveType
getOffenseItemMultiplier _ _ moveType _ "meadowplate" = getPlateMultiplier GRASS moveType
getOffenseItemMultiplier _ _ moveType _ "electricgem" = getGemMultiplier ELECTRIC moveType
getOffenseItemMultiplier _ _ moveType _ "magnet" = getItemTypeMultiplier ELECTRIC moveType
getOffenseItemMultiplier _ _ moveType _ "zapplate" = getPlateMultiplier ELECTRIC moveType
getOffenseItemMultiplier _ _ moveType _ "fairygem" = getGemMultiplier FAIRY moveType
getOffenseItemMultiplier _ _ moveType _ "pixieplate" = getPlateMultiplier FAIRY moveType
getOffenseItemMultiplier _ _ moveType _ "buggem" = getGemMultiplier BUG moveType
getOffenseItemMultiplier _ _ moveType _ "silverpowder" = getItemTypeMultiplier BUG moveType
getOffenseItemMultiplier _ _ moveType _ "insectplate" = getPlateMultiplier BUG moveType
getOffenseItemMultiplier _ _ moveType _ "dragongem" = getGemMultiplier DRAGON moveType
getOffenseItemMultiplier _ _ moveType _ "dragonfang" = getItemTypeMultiplier DRAGON moveType
getOffenseItemMultiplier _ _ moveType _ "dracoplate" = getPlateMultiplier DRAGON moveType
getOffenseItemMultiplier _ _ moveType _ "chilanberry" = getBerryMultiplier NORMAL moveType
getOffenseItemMultiplier _ _ _ _ _ = 1

getDefenseItemMultiplier :: Typing -> AttackRelation -> T.Text -> T.Text -> (T.Text, T.Text) -> Double
getDefenseItemMultiplier moveType SuperEffective _ "babiriberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier STEEL moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "babiriberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier STEEL moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "chartiberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ROCK moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "chartiberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ROCK moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "chopleberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FIGHTING moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "chopleberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FIGHTING moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "cobaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FLYING moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "cobaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FLYING moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "colburberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier DARK moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "colburberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier DARK moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "habanberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier DRAGON moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "habanberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier DRAGON moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "kasibberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GHOST moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "kasibberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GHOST moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "kebiaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier POISON moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "kebiaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier POISON moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "occaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FIRE moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "occaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FIRE moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "passhoberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier WATER moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "passhoberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier WATER moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "payapaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier PSYCHIC moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "payapaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier PSYCHIC moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "rindoberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GRASS moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "rindoberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GRASS moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "shucaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GROUND moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "shucaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GROUND moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "tangaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier BUG moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "tangaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier BUG moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "wacanberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ELECTRIC moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "wacanberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ELECTRIC moveType else 1
getDefenseItemMultiplier moveType SuperEffective _ "yacheberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ICE moveType else 1
getDefenseItemMultiplier moveType StronglyEffective _ "yacheberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ICE moveType else 1
getDefenseItemMultiplier _ _ _ _ _ = 1

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
isGrounded EP {..} = not flying
  where
    ts = epTyping
    ability = epAbilityId
    item = fromMaybe "" (epItem <&> toId . iId)
    flying = FLYING `elem` ts || ability == "levitate" || item == "airballoon" || epRisen

willCrit :: EffectivePokemon -> EffectivePokemon -> Bool
willCrit EP {epAbilityId = "merciless"} EP {epStatus = Just x} = True
willCrit _ _ = False

isBurned :: EffectivePokemon -> Bool
isBurned EP {epStatus = Just BURN} = True
isBurned _ = False

hasItem :: T.Text -> EffectivePokemon -> Bool
hasItem name EP {epItem = Just x} = iId x == name
hasItem _ _ = False

canUseItem :: EffectivePokemon -> Environment -> Bool
canUseItem _ Env {magicRoom = True} = False
canUseItem EP {epAbilityId = "klutz"} _ = False
canUseItem _ _ = True

canUseBerry :: EffectivePokemon -> EffectivePokemon -> Environment -> Bool
canUseBerry berryUser def env = canUseItem berryUser env && epAbilityId def /= "unnerve"

isMisty :: Environment -> Bool
isMisty Env {activeTerrain = Just MISTY} = True
isMisty _ = False

hasWeather :: Weather -> Environment -> Bool
hasWeather weather Env {activeWeather = Just w} = weather == w
hasWeather _ _ = False