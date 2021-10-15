{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pokemon.Types where

import Data.Char
import Data.Ratio
import qualified Data.Text as T
import Data.StatMultiplier (StatMultiplier)

-- | Wrapper around all different data types for the /dt command from showdown
data DTType = DtPokemon Pokemon | DtItem Item | DtMove Move | DtNature Nature | DtAbility Ability

instance Show DTType where
  show (DtPokemon x) = show x
  show (DtItem x) = show x
  show (DtMove x) = show x
  show (DtAbility x) = show x
  show (DtNature x) = show x

type Name = String

type Description = String

-- | Ability contains the name of the ability and it's description
data Ability = Ability Name (Maybe Description)

instance Show Ability where
  show (Ability name (Just description)) = name ++ ": " ++ description
  show (Ability name _) = name ++ ": no description yet in the api."

-- | Item contains the name of an item and it's description
data Item = Item Name (Maybe Description) deriving Eq

instance Show Item where
  show (Item name (Just description)) = name ++ ": " ++ description
  show (Item name _) = name ++ ": no description yet in the api."

-- | Data type representing a move, dClass is either physical or special, bp can be battle power, accuracy is only applicable to moves that have accuracy
data Move = Move
  { mName :: Name,
    mTipe :: Type,
    mDClass :: AttackType,
    mBp :: Maybe Int,
    mAccuracy :: Maybe Int,
    mDescription :: Maybe Description
  }
  deriving (Show, Eq)

-- | Attack type
data AttackType = PHYSICAL | SPECIAL | OTHER deriving (Eq, Ord, Show)

-- | Nature datatype, contains the name of the nature, the positive stat increase and then the negative. If the stats are neutral then there is no change.
data Nature = Nature String Stat Stat deriving (Eq)

-- | Effect of a nature on a stat, used to simplify functions that calc evs
data NatureEffect = NNegative | NNeutral | NPositive deriving (Show, Eq)

instance Show Nature where
  show (Nature name NEUTRAL NEUTRAL) = name ++ ": No effects on stats"
  show (Nature name positive negative) = name ++ ": 10 % increase for " ++ show positive ++ " and 10% decrease for " ++ show negative

-- | All different stats for a pokemon
data Stat = HP | ATK | DEF | SPATK | SPDEF | SPEED | NEUTRAL deriving (Eq)

instance Show Stat where
  show HP = "hp"
  show ATK = "attack"
  show DEF = "defense"
  show SPATK = "special attack"
  show SPDEF = "special defense"
  show SPEED = "speed"
  show NEUTRAL = "neutral"

-- | Data type for a pokemon stat, the stat it represents and the value of that stat
data BaseStat = BaseStat Stat Int deriving (Show, Eq)

-- | List of base stats
type BaseStats = [BaseStat]

-- | List of Pokemon types
type Typing = [Type]

-- | Pokemon types
data Type
  = NORMAL
  | FIGHTING
  | FLYING
  | POISON
  | GROUND
  | ROCK
  | BUG
  | GHOST
  | STEEL
  | FIRE
  | WATER
  | GRASS
  | ELECTRIC
  | PSYCHIC
  | ICE
  | DRAGON
  | DARK
  | FAIRY
  deriving (Show, Eq)

-- | Make read be able to parse strings into types, "normal" -> NORMAL
instance Read Type where
  readsPrec _ input = case map toLower input of
    'n' : 'o' : 'r' : 'm' : 'a' : 'l' : rest -> [(NORMAL, rest)]
    'f' : 'i' : 'g' : 'h' : 't' : 'i' : 'n' : 'g' : rest -> [(FIGHTING, rest)]
    'f' : 'l' : 'y' : 'i' : 'n' : 'g' : rest -> [(FLYING, rest)]
    'p' : 'o' : 'i' : 's' : 'o' : 'n' : rest -> [(POISON, rest)]
    'g' : 'r' : 'o' : 'u' : 'n' : 'd' : rest -> [(GROUND, rest)]
    'r' : 'o' : 'c' : 'k' : rest -> [(ROCK, rest)]
    'b' : 'u' : 'g' : rest -> [(BUG, rest)]
    'g' : 'h' : 'o' : 's' : 't' : rest -> [(GHOST, rest)]
    's' : 't' : 'e' : 'e' : 'l' : rest -> [(STEEL, rest)]
    'f' : 'i' : 'r' : 'e' : rest -> [(FIRE, rest)]
    'w' : 'a' : 't' : 'e' : 'r' : rest -> [(WATER, rest)]
    'g' : 'r' : 'a' : 's' : 's' : rest -> [(GRASS, rest)]
    'e' : 'l' : 'e' : 'c' : 't' : 'r' : 'i' : 'c' : rest -> [(ELECTRIC, rest)]
    'p' : 's' : 'y' : 'c' : 'h' : 'i' : 'c' : rest -> [(PSYCHIC, rest)]
    'i' : 'c' : 'e' : rest -> [(ICE, rest)]
    'd' : 'r' : 'a' : 'g' : 'o' : 'n' : rest -> [(DRAGON, rest)]
    'd' : 'a' : 'r' : 'k' : rest -> [(DARK, rest)]
    'f' : 'a' : 'i' : 'r' : 'y' : rest -> [(FAIRY, rest)]
    _ -> []

-- | Pokemon represented from pokemon api
-- Should contain more maybes to cover for data not being present in pokemon api
data Pokemon = Pokemon
  { pName :: Name,
    pTyping :: Typing,
    abilities :: [Name],
    hiddenAbilities :: Maybe Name,
    baseStats :: BaseStats,
    pMoves :: Either String [Move],
    weight :: Int
  }
  deriving (Show, Eq)

data PokemonS = PokemonS {
  pokemon :: Pokemon,
  pLevel :: Int,
  pItem :: Item,
  pNature :: Nature,
  pAbility :: T.Text,
  pEvs :: EVs,
  pIvs :: IVs,
  pMultiplier :: StatMultiplier
} deriving (Show, Eq)

data EVs = EVS {
  hpEv :: Int,
  atkEv :: Int,
  defEv :: Int,
  spatkEv :: Int,
  spdefEv :: Int,
  spdEv :: Int
} deriving (Show, Eq)

data IVs = IVS {
  hpIv :: Int,
  atkIv :: Int,
  defIv :: Int,
  spatkIv :: Int,
  spdefIv :: Int,
  spdIv :: Int
} deriving (Show, Eq)

-- | Level of a pokemon 0 - 100
type Level = Int

-- | Move types
data MoveCategory = STATUS | ATTACK | HAZARD | BOOST | UTILITY | RECOVERY | REST deriving (Eq, Show, Ord)

-- | Status types
data Status = BURN | PARALYZED | POISONED | SLEEP | FROZEN deriving (Show, Eq, Ord)

instance Read Status where
  readsPrec _ input = case map toLower input of
    'p' : 's' : 'n' : rest -> [(POISONED, rest)]
    'b' : 'r' : 'n' : rest -> [(BURN, rest)]
    't' : 'o' : 'x' : rest -> [(POISONED, rest)]
    'p' : 'a' : 'r' : rest -> [(PARALYZED, rest)]
    's' : 'l' : 'p' : rest -> [(SLEEP, rest)]
    'f' : 'r' : 'z' : rest -> [(FROZEN, rest)]
    _ -> []

-- | Weather
data Weather = SANDSTORM | HAIL | RAIN | SUN | HEAVYRAIN | HEAVYSUN deriving (Show, Eq, Ord)

-- | Terrain
data Terrain = ELECTRIC_T | PSYCHIC_T | MISTY | GRASSY deriving (Show, Eq, Ord)

-- | Hazards
data Hazards = SPIKES | TOXIC_SPIKES | STEALTH_ROCKS | STICKY_WEBS | VOLCALITH | CANNONADE | VINELASH | WILDFIRE deriving (Show, Eq, Ord)

data Volatile = LEECH_SEED | TAUNT | DESTINY_BOND | CONFUSION | YAWN deriving (Show, Eq, Ord)

data Screen = AURORA_VEIL | LIGHT_SCREEN | REFLECT deriving (Show, Eq, Ord)

data Environment = Env {
  activeTerrain :: Maybe Terrain,
  activeWeather :: Maybe Weather,
  screens :: [Screen],
  isDiving :: Bool,
  isMinimized :: Bool,
  isDigging :: Bool
} deriving (Show, Eq)