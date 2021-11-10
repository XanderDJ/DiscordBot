module Pokemon.DBConversion where

import qualified Data.Text as T
import Pokemon.Types
import qualified PokemonDB.Queries as Q
import PokemonDB.Types

toPokemon :: DBCompletePokemon  -> Pokemon
toPokemon (dbPoke, types, abilities, moves) =
  Pokemon
    { pName = pokemonName dbPoke,
      pNum = pokemonNum dbPoke,
      pTyping = map (read . T.unpack) types,
      baseStats = getBaseStats dbPoke,
      abilities = abilities,
      pMoves = Right (map toMove moves),
      pNfe = pokemonNFE dbPoke,
      pColour = pokemonColour dbPoke,
      pWeight = floor (pokemonWeight dbPoke)
    }

getBaseStats :: DBPokemon -> BaseStats
getBaseStats (PokemonT _ _ _ hpS atkS defS spaS spdS speS _ _ _) =
  [ BaseStat HP hpS,
    BaseStat ATK atkS,
    BaseStat DEF defS,
    BaseStat SPATK spaS,
    BaseStat SPDEF spdS,
    BaseStat SPEED speS
  ]

toMove :: DBMove -> Move
toMove dbMove = Move {
    mName = moveName dbMove,
    mTipe = (read . T.unpack . T.toLower . moveType) dbMove,
    mDClass = (toDamageClass . moveCategory) dbMove,
    mBp = (toMbp . moveBp) dbMove,
    mAccuracy = moveAccuracy dbMove,
    mDescription = Just $ moveDesc dbMove
}
 where
     toMbp bp = if bp == 0 then Nothing else Just bp
     toDamageClass "Status" = OTHER
     toDamageClass "Physical" = PHYSICAL 
     toDamageClass "Special" = SPECIAL
     toDamageClass other = OTHER 

toAbility :: DBAbility -> Ability
toAbility dbAbility = Ability (abilityName dbAbility) (Just (abilityDesc dbAbility))

toItem :: DBItem -> Item
toItem dbItem = Item (itemName dbItem) (Just (itemDesc dbItem)) (flingBp dbItem)

toDTType :: DBData -> DTType 
toDTType (DTI item) = DtItem (toItem item)
toDTType (DTA ability) = DtAbility (toAbility ability)
toDTType (DTM move) = DtMove (toMove move)
toDTType (DTP pokemon) = DtPokemon (toPokemon pokemon)