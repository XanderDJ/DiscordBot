module PokemonDB.Types where

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
  }
  deriving (Show)

type DBItem = ItemT Text Bool Int (Maybe Text)

newtype TypeT a = TypeT {typeName :: a} deriving (Show)

type DBType = TypeT Text

data AbilityT t n = AbilityT
  { abilityId :: t,
    abilityName :: t,
    abilityNum :: n,
    abilityDesc :: t
  }
  deriving (Show)

type DBAbility = AbilityT Text Int

data MoveT b n t mn mf mt = MoveT
  { moveId :: t,
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
    moveDrainPercentage :: mf,
    moveRecoil :: b,
    moveRecoilPercentage :: mf,
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
    movePulse :: b,
    movePunch :: b,
    moveBite :: b,
    movePowder :: b,
    moveDefrost :: b,
    moveSound :: b,
    moveDance :: b,
    moveWillCrit :: b
  }
  deriving (Show)

type DBMove = MoveT Bool Int Text (Maybe Int) (Maybe Double) (Maybe Text)

data PokemonT b d n t = PokemonT
  { pokemonId :: t,
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
  }
  deriving (Show)

type DBPokemon = PokemonT Bool Double Int Text

data PokemonMoveT t = PMT
  { pmPokemonId :: t,
    pmMoveId :: t
  }
  deriving (Show)

type PokemenMoveR = PokemonMoveT Text

data PokemonAbilityT t = PAT
  { paPokemonId :: t,
    paAbilityId :: t
  }
  deriving (Show)

type PokemonAbilityR = PokemonAbilityT Text

data PokemonTypeT t = PTT
  { ptPokemonId :: t,
    ptType :: t
  }
  deriving (Show)

type PokemonTypeR = PokemonTypeT Text

data OCT t = OCT
  { ocId :: t,
    ocClass :: t
  }
  deriving (Show)

type ObjectClass = OCT Text

type DBCompletePokemon = (DBPokemon, [TypeName], [AbilityName], [DBMove])

data DBData = DTP DBCompletePokemon | DTI DBItem | DTM DBMove | DTA DBAbility

instance Show DBData where
  show (DTP p) = show p
  show (DTA a) = show a
  show (DTI i) = show i
  show (DTM m) = show m

type TypeName = Text

type AbilityName = Text

type MoveName = Text

-- TYPES to make signatures more clearer
type PokemonId =
  -- | all lowercase, no hyphens, dashes
  Text

type MoveId =
  -- | All lowercase, no spaces, hyphens, dashes
  Text

type AbilityId =
  -- | All lowercase
  Text

type ItemId =
  -- | All lowercase
  Text

type ObjectId =
  -- | All lowercase no spaces
  Text

type Stat =
  -- | "hp", "atk", "def", "spa", "spd", "spe", others are not accepted
  Text

type MoveCategory =
  -- | Title case, "Status", "Special", "Physical" (Will always ensure first letter is capitalised when accepted)
  Text

type MoveType =
  -- | Title Case, examples: "Fire", "Water", "Grass"
  Text

data PokemonQuery
  = -- | Query if Pokemon learns some of the moves given
    Learn PokemonId [MoveId]
  | -- | Query information about the given objectId, can be a move, pokemon, ability or item
    DT ObjectId
  | -- | Query all the moves that a pokemon learns
    AllMoves PokemonId
  | -- | Query all the moves that a pokemon learns from a certain type
    AllMovesFromType PokemonId MoveType
  | -- | Query all the moves that a pokemon learns from a certain category
    AllMovesFromCategory PokemonId MoveCategory
  | -- | Query all the moves that a pokemon learns from a certain category and move type
    AllMovesFromCategoryAndType PokemonId MoveCategory MoveType
  | -- | Query all the pokemon that has a certain stat\n
    -- Stat = "hp", "atk", ..\n
    -- Int = the stat number i.e. 100
    AllPokemonWithStat Stat Int
  | -- | Query all the pokemon that can have the given ability
    AllPokemonsWithAbility AbilityId
  | -- | Query all the pokemon that can learn the given move
    AllPokemonsWithMove MoveId
  | -- | Query all the priority moves that a pokemon can learn
    AllPriorityMoves PokemonId
  | -- | Query all the hazards that a mon can learn
    AllHazardMoves PokemonId
  | -- | Query all the recovery moves a mon can learn
    AllRecoveryMoves PokemonId
  | -- | Query all the cleric moves a mon can learn
    AllClericMoves PokemonId
  | -- | Query all the screens that a mon can put up
    AllScreens PokemonId
  | -- | Query all the hazard control moves a mon has
    AllHazardControl PokemonId
  | -- | Query all the SetUp moves a mon has
    AllSetUpMoves PokemonId
  | AllStatusMoves PokemonId -- ^Query all the status moves a mon has

data PokemonQueryResult
  = LearnR [DBMove]
  | -- | Gives DBData back if the give ObjectId was found. DBData is a union of all the data tagged.
    DTR (Maybe DBData)
  | AllMovesR [DBMove]
  | AllMovesFromTypeR [DBMove]
  | AllMovesFromCategoryR [DBMove]
  | AllMovesFromCategoryAndTypeR [DBMove]
  | AllPokemonWithStatR [DBPokemon]
  | AllPokemonsWithAbilityR [DBPokemon]
  | AllPokemonsWithMoveR [DBPokemon]
  | AllPriorityMovesR [DBMove]
  | AllHazardMovesR [DBMove]
  | AllRecoveryMovesR [DBMove]
  | AllClericMovesR [DBMove]
  | AllScreensR [DBMove]
  | AllHazardControlR [DBMove]
  | AllSetUpMovesR [DBMove]
  | AllStatusMovesR [DBMove]