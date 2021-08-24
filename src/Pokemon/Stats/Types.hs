module Pokemon.Stats.Types where

import Data.Default
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Pokemon.Replays.Types hiding (Name)
import Pokemon.Types hiding (Name)

-- Individual stats for a pokemon in one match
type UtilityFactor = Double

type AccumulateHazardDmg = Double

type SelfDestructs = Int

type HazardDamageTaken = Double

--  Aggregate stats for a pokemon in multiple matches
type Turns = Double

type Kills = Int

type IndirectKills = Int

type TimesBrought = Int

type TimesDied = Int

type Name = T.Text

type Nickname = T.Text 

data PokeStats = PokeStats
  { accumulateHazardDmg :: AccumulateHazardDmg,
    selfDestructs :: SelfDestructs,
    hzrdDmgTaken :: HazardDamageTaken,
    kills :: Kills,
    indirectKills :: IndirectKills,
    timesDied :: TimesDied,
    timesBrought :: TimesBrought,
    turnsActive :: Int,
    turnsUtil :: Int,
    utilityFactor :: UtilityFactor,
    movesMissed :: Int,
    movesMade :: Int,
    accuracy :: Float
  }
  deriving (Show)

data PlayerStats = PlayerStats
  { pName :: T.Text,
    totalTurns :: Turns,
    pokeStats :: M.Map Name PokeStats
  }
  deriving (Show)

data Match = Match
  { players :: M.Map Position PlayerStats,
    currentMajorAction :: ReplayMessage,
    outcome :: ReplayMessage,
    weather :: M.Map Weather Nickname,
    terrain :: M.Map Terrain Nickname,
    hazards :: M.Map ((Position, Position), Hazards) Nickname,
    nicks :: M.Map Nickname Name,
    pokeHealth :: M.Map Nickname Health,
    pokeStatusCause :: M.Map (Nickname, Status) Nickname,
    utilityMap :: M.Map Nickname Bool,
    diedMap :: M.Map Position Bool
  }
  deriving (Show)

instance Default PokeStats where
  def = PokeStats 0 0 0 0 0 0 1 0 0 0 0 0 1

instance Default PlayerStats where
  def = PlayerStats "" 0 M.empty

instance Default Match where
  def = Match M.empty (RMD DELIMITER) (RMD DELIMITER) M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty

instance Semigroup PokeStats where
  (<>) ps1 ps2 =
    PokeStats
      accumulateHazardDmg'
      selfDestructs'
      hazardDamageTaken'
      kills'
      indirectKills'
      timesDied'
      timesBrought'
      turnsActive'
      turnsUtil'
      utilityFactor'
      movesMissed'
      movesMade'
      accuracy'
    where
      accumulateHazardDmg' = average (accumulateHazardDmg ps1) (accumulateHazardDmg ps2)
      selfDestructs' = selfDestructs ps1 + selfDestructs ps2
      hazardDamageTaken' = average (hzrdDmgTaken ps1) (hzrdDmgTaken ps2)
      kills' = kills ps1 + kills ps2
      indirectKills' = indirectKills ps1 + indirectKills ps2
      timesDied' = timesDied ps1 + timesDied ps2
      timesBrought' = timesBrought ps1 + timesBrought ps2
      turnsActive' = turnsActive ps1 + turnsActive ps2
      turnsUtil' = turnsUtil ps1 + turnsUtil ps2
      utilityFactor' = average (utilityFactor ps1) (utilityFactor ps2)
      movesMade' = movesMade ps1 + movesMade ps2
      movesMissed' = movesMissed ps1 + movesMissed ps2
      accuracy' = average (accuracy ps1) (accuracy ps2)

-- | If the name of both playerstats are equal then the two will be combined with the total turns added and the pokestats being combined
combine :: PlayerStats -> PlayerStats -> PlayerStats
combine (PlayerStats name1 turns1 map1) (PlayerStats name2 turns2 map2) = if name1 == name2 then PlayerStats name1 (average turns1 turns2) (combineMaps map1 map2) else PlayerStats name1 turns1 map1

combineMaps :: (Ord a, Semigroup b) => M.Map a b-> M.Map a b -> M.Map a b
combineMaps = M.unionWith (<>)

average :: (Num a, Fractional a) => a -> a -> a
average x y = (x + y) / 2
