module Pokemon.Types where

import Data.Char
import Data.Ratio
import Data.StatMultiplier (StatMultiplier)
import Data.Text (Text)
import qualified Data.Text as T

-- | Wrapper around all different data types for the /dt command from showdown
data DTType = DtPokemon Pokemon | DtItem Item | DtMove Move | DtNature Nature | DtAbility Ability

instance Show DTType where
  show (DtPokemon x) = show x
  show (DtItem x) = show x
  show (DtMove x) = show x
  show (DtAbility x) = show x
  show (DtNature x) = show x

type Name = Text

type Description = Text

-- | Ability contains the name of the ability and it's description
data Ability = Ability Name (Maybe Description)

instance Show Ability where
  show (Ability name (Just description)) = show name ++ ": " ++ show description
  show (Ability name _) = show name ++ ": no description yet in the api."

-- | Item contains the name of an item and it's description
data Item = Item
  { iName :: Name,
    iDesc :: Maybe Description,
    iBerry :: Bool,
    iFlingBp :: Int,
    iOnPlate :: Maybe T.Text,
    iOnDrive :: Maybe T.Text,
    iOnMemory :: Maybe T.Text
  }
  deriving (Eq)

instance Show Item where
  show (Item name (Just description) _ bp _ _ _) = show name ++ "(flingBP=" ++ show bp ++ "): " ++ show description
  show (Item name _ _ bp _ _ _) = show name ++ ": no description yet in the api."

-- | Data type representing a move, dClass is either physical or special, bp can be battle power, accuracy is only applicable to moves that have accuracy
data Move = Move
  { mName :: Name,
    mTipe :: Type,
    mDClass :: AttackType,
    mBp :: Maybe Int,
    mPrio :: Int,
    mAccuracy :: Maybe Int,
    mDescription :: Maybe Description,
    mFlags :: [Text]
  }
  deriving (Show, Eq)

instance Ord Move where
  (Move _ _ _ (Just b) _ _ _ _) <= (Move _ _ _ (Just b') _ _ _ _) = b <= b'
  (Move _ _ _ (Just b) _ _ _ _) <= (Move _ _ _ Nothing _ _ _ _) = False
  (Move _ _ _ Nothing _ _ _ _) <= (Move _ _ _ (Just b') _ _ _ _) = True
  (Move _ _ _ Nothing _ _ _ _) <= (Move _ _ _ Nothing _ _ _ _) = True

-- | Attack type
data AttackType = PHYSICAL | SPECIAL | OTHER deriving (Eq, Ord, Show)

-- | Nature datatype, contains the name of the nature, the positive stat increase and then the negative. If the stats are neutral then there is no change.
data Nature = Nature String Stat Stat deriving (Eq)

-- | Effect of a nature on a stat, used to simplify functions that calc evs
data NatureEffect = NNegative | NNeutral | NPositive deriving (Show, Eq)

instance Show Nature where
  show (Nature name NEU NEU) = name ++ ": No effects on stats"
  show (Nature name positive negative) = name ++ ": 10% increase for " ++ show positive ++ " and 10% decrease for " ++ show negative

-- | All different stats for a pokemon
data Stat = HP | ATK | DEF | SPA | SPD | SPE | NEU deriving (Eq)

instance Show Stat where
  show HP = "Hp"
  show ATK = "Attack"
  show DEF = "Defense"
  show SPA = "Special attack"
  show SPD = "Special defense"
  show SPE = "Speed"
  show NEU = "neutral"

-- | Data type for a pokemon stat, the stat it represents and the value of that stat
data BaseStat = BaseStat Stat Int deriving (Eq)

instance Show BaseStat where
  show (BaseStat name val) = show name ++ ": " ++ show val

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
  deriving (Show, Eq, Ord)

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
    pNum :: Int,
    pTyping :: Typing,
    abilities :: [Name],
    baseStats :: BaseStats,
    pMoves :: Either String [Move],
    pColour :: Name,
    pNfe :: Bool,
    pWeight :: Int
  }
  deriving (Eq)

instance Show Pokemon where
  show (Pokemon name num types abilities bs _ colour nfe weight) =
    "Pokemon "
      ++ show name
      ++ " "
      ++ show types
      ++ " "
      ++ show abilities
      ++ " "
      ++ show bs
      ++ " colour="
      ++ show colour
      ++ " weight="
      ++ show weight

-- | Level of a pokemon 0 - 100
type Level = Int

-- | Move types
data MoveCategory = STATUS | ATTACK | HAZARD | BOOST | UTILITY | RECOVERY | REST deriving (Eq, Show, Ord)

-- | Status types
data Status = BURN | PARALYZED | POISONED | SLEEP | FROZEN | BADLY_POISONED deriving (Show, Eq, Ord)

instance Read Status where
  readsPrec _ input = case map toLower input of
    'p' : 's' : 'n' : rest -> [(POISONED, rest)]
    'p' : 'o' : 'i' : 's' : 'o' : 'n' : 'e' : 'd' : rest -> [(POISONED, rest)]
    'b' : 'r' : 'n' : rest -> [(BURN, rest)]
    'b' : 'u' : 'r' : 'n' : rest -> [(BURN, rest)]
    'b' : 'u' : 'r' : 'n' : 'e' : 'd' : rest -> [(BURN, rest)]
    't' : 'o' : 'x' : rest -> [(BADLY_POISONED, rest)]
    't' : 'o' : 'x' : 'i' : 'c' : rest -> [(BADLY_POISONED, rest)]
    'p' : 'a' : 'r' : 'a' : 'l' : 'y' : 'z' : 'e' : 'd' : rest -> [(PARALYZED, rest)]
    'p' : 'a' : 'r' : rest -> [(PARALYZED, rest)]
    's' : 'l' : 'p' : rest -> [(SLEEP, rest)]
    's' : 'l' : 'e' : 'e' : 'p' : rest -> [(SLEEP, rest)]
    'f' : 'r' : 'z' : rest -> [(FROZEN, rest)]
    'f' : 'r' : 'o' : 'z' : 'e' : 'n' : rest -> [(FROZEN, rest)]
    _ -> []

-- | Weather
data Weather = SANDSTORM | HAIL | RAIN | SUN | HEAVYRAIN | HEAVYSUN | STRONGWIND deriving (Eq, Ord)

instance Show Weather where
  show w = case w of
    SANDSTORM -> "Sand"
    HAIL -> "Hail"
    RAIN -> "Rain"
    SUN -> "Sun"
    HEAVYRAIN -> "Heavy Rain"
    HEAVYSUN -> "Harsh sun"
    STRONGWIND -> "Strong Winds"

parseWeather :: T.Text -> Maybe Weather
parseWeather "sand" = Just SANDSTORM
parseWeather "sandstorm" = Just SANDSTORM
parseWeather "hail" = Just HAIL
parseWeather "rain" = Just RAIN
parseWeather "sun" = Just SUN
parseWeather "primordialsea" = Just HEAVYRAIN
parseWeather "primordial" = Just HEAVYRAIN
parseWeather "desolateland" = Just HEAVYSUN
parseWeather "desolate" = Just HEAVYSUN
parseWeather "deltastream" = Just STRONGWIND
parseWeather "delta" = Just STRONGWIND
parseWeather "strongwind" = Just STRONGWIND
parseWeather _ = Nothing

-- | Terrain
data Terrain = ELECTRIC_T | PSYCHIC_T | MISTY | GRASSY deriving (Eq, Ord)

instance Show Terrain where
  show terrain = case terrain of 
    ELECTRIC_T -> "Electric Terrain"
    PSYCHIC_T -> "Psychic Terrain"
    MISTY -> "Misty Terrain"
    GRASSY -> "Grassy Terrain"

parseTerrain :: T.Text -> Maybe Terrain
parseTerrain "electricterrain" = Just ELECTRIC_T
parseTerrain "elec" = Just ELECTRIC_T
parseTerrain "electric" = Just ELECTRIC_T
parseTerrain "grassyterrain" = Just GRASSY
parseTerrain "grassy" = Just GRASSY
parseTerrain "mistyterrain" = Just MISTY
parseTerrain "misty" = Just MISTY
parseTerrain "psychicterrain" = Just PSYCHIC_T
parseTerrain "psychic" = Just PSYCHIC_T
parseTerrain "psy" = Just PSYCHIC_T
parseTerrain _ = Nothing

data Screen = AURORA_VEIL | LIGHT_SCREEN | REFLECT deriving (Show, Eq, Ord)

parseScreen :: T.Text -> Maybe Screen
parseScreen "reflect" = Just REFLECT
parseScreen "lightscreen" = Just LIGHT_SCREEN
parseScreen "light" = Just LIGHT_SCREEN
parseScreen "auroraveil" = Just AURORA_VEIL
parseScreen "aurora" = Just AURORA_VEIL
parseScreen _ = Nothing

-- | Hazards
data Hazards = SPIKES | TOXIC_SPIKES | STEALTH_ROCKS | STICKY_WEBS | VOLCALITH | CANNONADE | VINELASH | WILDFIRE deriving (Show, Eq, Ord)

data Volatile = LEECH_SEED | TAUNT | DESTINY_BOND | CONFUSION | YAWN deriving (Show, Eq, Ord)
