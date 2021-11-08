{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module PokemonDB.Tables where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Opaleye
import PokemonDB.Types

type ItemF = ItemT (Field SqlText) (Field SqlBool) (Field SqlInt4) (FieldNullable SqlText)

$(makeAdaptorAndInstance "pItem" ''ItemT)

itemTable :: Table ItemF ItemF
itemTable =
  table
    "item"
    ( pItem
        ItemT
          { itemId = tableField "id",
            itemName = tableField "name",
            itemNum = tableField "num",
            itemDesc = tableField "desc",
            isChoice = tableField "isChoice",
            isBerry = tableField "isBerry",
            isGem = tableField "isGem",
            isPokeball = tableField "isPokeball",
            ignoreKlutz = tableField "ignoreKlutz",
            flingBp = tableField "flingBp",
            onPlate = tableField "onPlate",
            onMemory = tableField "onMemory",
            onDrive = tableField "onDrive"
          }
    )

selectItem :: Select ItemF
selectItem = selectTable itemTable

type TypeF = TypeT (Field SqlText)

$(makeAdaptorAndInstance "pType" ''TypeT)

typeTable :: Table TypeF TypeF
typeTable =
  table
    "type"
    ( pType
        TypeT
          { typeName = tableField "type"
          }
    )

selectType :: Select TypeF
selectType = selectTable typeTable

type AbilityF = AbilityT (Field SqlText) (Field SqlInt4)

$(makeAdaptorAndInstance "pAbility" ''AbilityT)

abilityTable :: Table AbilityF AbilityF
abilityTable =
  table
    "ability"
    ( pAbility
        AbilityT
          { abilityId = tableField "id",
            abilityName = tableField "name",
            abilityNum = tableField "num",
            abilityDesc = tableField "desc"
          }
    )

selectAbility :: Select AbilityF
selectAbility = selectTable abilityTable

type MoveF = MoveT (Field SqlBool) (Field SqlInt4) (Field SqlText) (FieldNullable SqlInt4) (FieldNullable SqlText)

$(makeAdaptorAndInstance "pMove" ''MoveT)

moveTable :: Table MoveF MoveF
moveTable =
  table
    "move"
    ( pMove
        MoveT
          { moveId = tableField "id",
            moveName = tableField "name",
            moveNum = tableField "num",
            moveBp = tableField "bp",
            moveAccuracy = tableField "accuracy",
            movePP = tableField "pp",
            movePriority = tableField "priority",
            moveDrain = tableField "drain",
            moveCategory = tableField "category",
            moveType = tableField "type",
            moveContact = tableField "contact",
            moveSecondaryChance = tableField "secondaryChance",
            moveDesc = tableField "desc",
            moveUTO = tableField "useTargetOffensive",
            moveUDAO = tableField "useDefensiveAsOffensive",
            moveINO = tableField "ignoreNegativeOffensive",
            moveIPD = tableField "ignorePositiveDefensive",
            moveIO = tableField "ignoreOffensive",
            moveID = tableField "ignoreDefensive",
            moveII = tableField "ignoreImmunity",
            moveIsZ = tableField "isZ",
            moveIsMax = tableField "isMax",
            moveWeather = tableField "weather",
            moveStatus = tableField "status",
            moveVolatileStatus = tableField "volatilestatus",
            moveBullet = tableField "bullet",
            movePunch = tableField "punch",
            moveBite = tableField "bite",
            movePowder = tableField "powder",
            moveDefrost = tableField "defrost",
            moveSound = tableField "sound",
            moveDance = tableField "dance",
            moveWillCrit = tableField "willCrit"
          }
    )

selectMove :: Select MoveF
selectMove = selectTable moveTable

type PokemonF = PokemonT (Field SqlBool) (Field SqlFloat8) (Field SqlInt4) (Field SqlText)

$(makeAdaptorAndInstance "pPokemon" ''PokemonT)

pokemonTable :: Table PokemonF PokemonF
pokemonTable =
  table
    "pokemon"
    ( pPokemon
        PokemonT
          { pokemonId = tableField "id",
            pokemonName = tableField "name",
            pokemonNum = tableField "num",
            pokemonHp = tableField "hp",
            pokemonAtk = tableField "atk",
            pokemonDef = tableField "def",
            pokemonSpa = tableField "spa",
            pokemonSpd = tableField "spd",
            pokemonSpe = tableField "spe",
            pokemonWeight = tableField "weight",
            pokemonColour = tableField "colour",
            pokemonNFE = tableField "nfe"
          }
    )

selectPokemon :: Select PokemonF
selectPokemon = selectTable pokemonTable

type PokemonMoveF = PokemonMoveT (Field SqlText)

type PokemonAbilityF = PokemonAbilityT (Field SqlText)

type PokemonTypeF = PokemonTypeT (Field SqlText)

$(makeAdaptorAndInstance "pPokemonMove" ''PokemonMoveT)
$(makeAdaptorAndInstance "pPokemonAbility" ''PokemonAbilityT)
$(makeAdaptorAndInstance "pPokemonType" ''PokemonTypeT)

pokemonMoveTable :: Table PokemonMoveF PokemonMoveF
pokemonMoveTable =
  table
    "pokemon_moves_move"
    ( pPokemonMove
        PMT
          { pmPokemonId = tableField "pokemonId",
            pmMoveId = tableField "moveId"
          }
    )

selectPokemonMove :: Select PokemonMoveF
selectPokemonMove = selectTable pokemonMoveTable

pokemonAbilityTable :: Table PokemonAbilityF PokemonAbilityF
pokemonAbilityTable =
  table
    "pokemon_abilities_ability"
    ( pPokemonAbility
        PAT
          { paPokemonId = tableField "pokemonId",
            paAbilityId = tableField "abilityId"
          }
    )

selectPokemonAbility :: Select PokemonAbilityF
selectPokemonAbility = selectTable pokemonAbilityTable

pokemonTypeTable :: Table PokemonTypeF PokemonTypeF
pokemonTypeTable =
  table
    "pokemon_types_type"
    ( pPokemonType
        PTT
          { ptPokemonId = tableField "pokemonId",
            ptType = tableField "typeType"
          }
    )

selectPokemonType :: Select PokemonTypeF
selectPokemonType = selectTable pokemonTypeTable

type OCF = OCT (Field SqlText)

$(makeAdaptorAndInstance "pOC" ''OCT)

ocTable :: Table OCF OCF
ocTable = table "object_class" (pOC OCT {
  ocId = tableField "id",
  ocClass = tableField "class"
})

selectOC :: Select OCF
selectOC = selectTable ocTable