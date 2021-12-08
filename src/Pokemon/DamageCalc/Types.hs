{-# LANGUAGE RecordWildCards #-}

module Pokemon.DamageCalc.Types where

import qualified Data.List as L
import Data.StatMultiplier
import qualified Data.Text as T
import Pokemon.Types

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
  { emName :: T.Text,
    emBp :: Int,
    emPriority :: Int,
    emCategory :: AttackType,
    emType :: Type,
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
    emHits :: Int
  }
  deriving (Eq, Show)

data EffectivePokemon = EP
  { epName :: T.Text,
    epAbility :: T.Text,
    epTyping :: [Type],
    epStats :: BaseStats,
    epLevel :: Int,
    epItem :: Maybe Item,
    epNature :: Nature,
    epEvs :: EVs,
    epIvs :: IVs,
    epStatus :: Maybe Status,
    epNfe :: Bool,
    epWeight :: Int,
    epRisen :: Bool,
    epMultiplier :: Multipliers,
    epHPPercentage :: Int
  }
  deriving (Show, Eq)

data EVs = EVS
  { hpEv :: Int,
    atkEv :: Int,
    defEv :: Int,
    spatkEv :: Int,
    spdefEv :: Int,
    spdEv :: Int
  }
  deriving (Show, Eq)

data IVs = IVS
  { hpIv :: Int,
    atkIv :: Int,
    defIv :: Int,
    spatkIv :: Int,
    spdefIv :: Int,
    spdIv :: Int
  }
  deriving (Show, Eq)

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
    protecting :: Bool,
    maxProtecting :: Bool,
    isDoubleBattle :: Bool
  }
  deriving (Show, Eq)

data MoveOrder = FIRST | LAST deriving (Show, Eq, Ord)