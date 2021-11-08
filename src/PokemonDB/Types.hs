module PokemonDB.Types  where

import Data.Text (Text)

data ItemT a b c d = ItemT
  { itemId :: a,
    itemName :: a,
    itemNum :: c,
    itemDesc :: a,
    isChoice :: b,
    isBerry :: b,
    isGem :: b,
    isPokeball :: b,
    ignoreKlutz :: b,
    flingBp :: c,
    onPlate :: d,
    onMemory :: d,
    onDrive :: d
  } deriving Show

type DBItem = ItemT Text Bool Int (Maybe Text)

newtype TypeT a = TypeT {typeName :: a} deriving Show

type DBType = TypeT Text

data AbilityT t n = AbilityT {
  abilityId :: t,
  abilityName :: t,
  abilityNum :: n,
  abilityDesc :: t
} deriving Show

type DBAbility = AbilityT Text Int

data MoveT b n t mn mt = MoveT {
  moveId :: t,
  moveName :: t,
  moveNum :: n,
  moveBp :: n,
  moveAccuracy :: mn,
  movePP :: n,
  movePriority :: n,
  moveCategory :: t,
  moveType :: t,
  moveContact :: b,
  moveSecondaryChance :: mn,
  moveDesc :: t,
  moveDrain :: b,
  moveUTO :: b,
  moveUDAO :: b,
  moveINO :: b,
  moveIPD :: b,
  moveIO :: b,
  moveID :: b,
  moveII :: b,
  moveIsZ :: b,
  moveIsMax :: b,
  moveWeather :: mt,
  moveVolatileStatus :: mt, 
  moveStatus :: mt,
  moveBullet :: b,
  movePunch :: b,
  moveBite :: b,
  movePowder :: b,
  moveDefrost :: b,
  moveSound :: b,
  moveDance :: b,
  moveWillCrit :: b
} deriving Show

type DBMove = MoveT Bool Int Text (Maybe Int) (Maybe Text)


data PokemonT b d n t = PokemonT {
  pokemonId :: t,
  pokemonName :: t,
  pokemonNum :: n,
  pokemonHp :: n,
  pokemonAtk :: n,
  pokemonDef :: n,
  pokemonSpa :: n,
  pokemonSpd :: n,
  pokemonSpe :: n,
  pokemonWeight :: d,
  pokemonColour :: t,
  pokemonNFE :: b
} deriving Show

type DBPokemon = PokemonT Bool Double Int Text

data PokemonMoveT t = PMT {
  pmPokemonId :: t,
  pmMoveId :: t
} deriving Show

type PokemenMoveR = PokemonMoveT Text

data PokemonAbilityT t = PAT {
  paPokemonId :: t,
  paAbilityId :: t
} deriving Show

type PokemonAbilityR = PokemonAbilityT Text


data PokemonTypeT  t= PTT {
  ptPokemonId :: t,
  ptType :: t
} deriving Show

type PokemonTypeR = PokemonTypeT Text

data OCT t = OCT {
  ocId :: t,
  ocClass :: t
} deriving Show

type ObjectClass = OCT Text

type DBCompletePokemon =  (DBPokemon, [TypeName], [AbilityName], [DBMove])

data DBData = DTP DBCompletePokemon | DTI DBItem | DTM DBMove | DTA DBAbility

instance Show DBData where
  show (DTP p) = show p
  show (DTA a) = show a
  show (DTI i) = show i
  show (DTM m) = show m 

type TypeName = Text
type AbilityName = Text
type MoveName = Text