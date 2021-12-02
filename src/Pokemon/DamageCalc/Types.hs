{-# LANGUAGE RecordWildCards #-}
module Pokemon.DamageCalc.Types where

import Data.StatMultiplier
import qualified Data.Text as T
import Pokemon.Types
import qualified Data.List as L

data DCS = DCS
  { dcsEnv :: Environment,
    dcsAttacker :: EffectivePokemon,
    dcsDefender :: EffectivePokemon,
    dcsMove :: EffectiveMove
  }
  deriving Eq

instance Show DCS where
  show DCS {..} = "DCS {\n" ++ L.intercalate "\n\n" ["ENV = " ++ show dcsEnv,"ATTACKER = " ++ show dcsAttacker,"DEFENDER = " ++ show dcsDefender,"MOVE = " ++ show dcsMove] ++ "\n}" 
  

data EffectiveMove = EM
  { emName :: T.Text,
    emBp :: Int,
    emCategory :: AttackType,
    emType :: Type,
    isContact :: Bool,
    emUTO :: Bool,
    emUDAO :: Bool,
    emINO :: Bool,
    emIPD :: Bool,
    emIO :: Bool,
    emID :: Bool,
    emII :: Bool,
    emIsMax :: Bool,
    emBullet :: Bool,
    emPunch :: Bool,
    emBite :: Bool,
    emPowder :: Bool,
    emSound :: Bool,
    emDance :: Bool,
    emWillCrit :: Bool
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
    epMultiplier :: StatMultiplier
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
    gravity :: Bool,
    magicRoom :: Bool,
    wonderRoom :: Bool,
    powerspot :: Bool,
    battery :: Bool,
    tailWind :: Bool,
    isDiving :: Bool,
    isMinimized :: Bool,
    isDigging :: Bool,
    protecting :: Bool,
    maxProtecting :: Bool
  }
  deriving (Show, Eq)