{-# LANGUAGE RecordWildCards #-}

module Pokemon.DamageCalc.Types where

import qualified Data.List as L
import Data.StatMultiplier
import qualified Data.Text as T
import Pokemon.Types
import Text.Printf


data DCS = DCS
  { dcsEnv :: Environment,
    dcsAttacker :: EffectivePokemon,
    dcsDefender :: EffectivePokemon,
    dcsMove :: EffectiveMove
  }
  deriving (Eq)

instance Show DCS where
  show DCS {..} = "DCS {\n" ++ L.intercalate "\n\n" ["ENV = " ++ show dcsEnv, "ATTACKER = " ++ show dcsAttacker, "DEFENDER = " ++ show dcsDefender, "MOVE = " ++ show dcsMove] ++ "\n}"

data Multipliers = Multipliers
  { atkM :: StatMultiplier,
    defM :: StatMultiplier,
    spaM :: StatMultiplier,
    spdM :: StatMultiplier,
    speM :: StatMultiplier
  }
  deriving (Eq, Show)

data EffectiveMove = EM
  { emId :: T.Text,
    emName :: T.Text,
    emBp :: Int,
    emPriority :: Int,
    emCategory :: AttackType,
    emType :: Type,
    emHasSecondary :: Bool,
    emDrain :: Bool,
    emDrainPercentage :: Maybe Double,
    emRecoil :: Bool,
    emRecoilPercentage :: Maybe Double,
    isContact :: Bool,
    hasSecondary :: Bool,
    emUTO :: Bool,
    emUDAO :: Bool,
    emINO :: Bool,
    emIPD :: Bool,
    emIO :: Bool,
    emID :: Bool,
    emII :: Bool,
    emIsMax :: Bool,
    emBullet :: Bool,
    emPulse :: Bool,
    emPunch :: Bool,
    emBite :: Bool,
    emPowder :: Bool,
    emSound :: Bool,
    emDance :: Bool,
    emWillCrit :: Bool,
    emTimesUsed :: Int,
    emHits :: Int,
    emStockPile :: Maybe Int
  }
  deriving (Eq, Show)

data EffectivePokemon = EP
  { epId :: T.Text,
    epName :: T.Text,
    epAbility :: T.Text,
    epAbilityId :: T.Text,
    epTyping :: [Type],
    epStats :: BaseStats,
    epLevel :: Int,
    epGender :: Gender,
    epItem :: Maybe Item,
    epNature :: Nature,
    epEvs :: EVs,
    epIvs :: IVs,
    epStatus :: Maybe Status,
    epNfe :: Bool,
    epWeight :: Int,
    epRisen :: Bool,
    epMultiplier :: Multipliers,
    epHPPercentage :: Double,
    epStatsLowered :: Bool,
    epFlashFire :: Bool
  }
  deriving (Show, Eq)

data EVs = EVS
  { hpEv :: Int,
    atkEv :: Int,
    defEv :: Int,
    spaEv :: Int,
    spdEv :: Int,
    speEv :: Int
  }
  deriving (Show, Eq)

data IVs = IVS
  { hpIv :: Int,
    atkIv :: Int,
    defIv :: Int,
    spaIv :: Int,
    spdIv :: Int,
    speIv :: Int
  }
  deriving (Show, Eq)

data EffectiveStats = ES
  { hpStat :: Int,
    atkStat :: Int,
    defStat :: Int,
    spaStat :: Int,
    spdStat :: Int,
    speStat :: Int
  } deriving Show

data Environment = Env
  { activeTerrain :: Maybe Terrain,
    activeWeather :: Maybe Weather,
    screens :: [Screen],
    crit :: Bool,
    gravity :: Bool,
    magicRoom :: Bool,
    wonderRoom :: Bool,
    trickRoom :: Bool,
    powerspot :: Bool,
    battery :: Bool,
    electrified :: Bool,
    tailWind :: Bool,
    isMinimized :: Bool,
    isInvulnerable :: Bool,
    luckyChant :: Bool,
    hitBefore :: Bool,
    switchingOut :: Bool,
    protecting :: Bool,
    maxProtecting :: Bool,
    isDoubleBattle :: Bool
  }
  deriving (Show, Eq)

data MoveOrder = FIRST | LAST deriving (Show, Eq, Ord)

data CalcResult = CalcResult
  { dmg :: (Int, Int),
    hpPercentage :: (Double, Double)
  }

printPercentage :: (PrintfArg a, Floating a) => Int -> a -> String
printPercentage = printf "%0.*f%%"

instance Show CalcResult where
  show CalcResult {..} = "minHP = " ++ show (fst dmg) ++ ", maxHP = " ++ show (snd dmg) ++ ", min% = " ++ printPercentage 1 (fst hpPercentage * 100) ++ ", max% = " ++ printPercentage 1 (snd hpPercentage * 100)

getMinDmg :: CalcResult -> Int
getMinDmg CalcResult {..} = fst dmg

getMaxDmg :: CalcResult -> Int
getMaxDmg CalcResult {..} = snd dmg

getMinPercent :: CalcResult -> String
getMinPercent CalcResult {..} = printPercentage 1 (fst hpPercentage * 100)

getMaxPercent :: CalcResult -> String
getMaxPercent CalcResult {..} = printPercentage 1 (snd hpPercentage * 100)