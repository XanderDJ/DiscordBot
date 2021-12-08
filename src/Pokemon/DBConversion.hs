{-# LANGUAGE RecordWildCards #-}

module Pokemon.DBConversion where

import qualified Data.Text as T
import Pokemon.Types
import qualified PokemonDB.Queries as Q
import PokemonDB.Types
import Pokemon.DamageCalc.Types
import Data.Maybe

toPokemon :: DBCompletePokemon -> Pokemon
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
toMove dbMove =
  Move
    { mName = moveName dbMove,
      mTipe = (read . T.unpack . T.toLower . moveType) dbMove,
      mDClass = (toDamageClass . moveCategory) dbMove,
      mBp = (toMbp . moveBp) dbMove,
      mAccuracy = moveAccuracy dbMove,
      mDescription = Just $ moveDesc dbMove,
      mFlags = getFlags dbMove
    }
  where
    toMbp bp = if bp == 0 then Nothing else Just bp
    toDamageClass "Status" = OTHER
    toDamageClass "Physical" = PHYSICAL
    toDamageClass "Special" = SPECIAL
    toDamageClass other = OTHER

getFlags :: DBMove -> [T.Text]
getFlags dbMove = getList
  [ (isBullet, "Bullet"),
    (isPulse, "Pulse"),
    (isDrain, "Draining"),
    (isBite, "Bite"),
    (isPunch, "Punch"),
    (isPowder, "Powder"),
    (willCrit, "Will crit"),
    (isSound, "Sound"),
    (isDance, "Dance"),
    (willDefrost, "Will thaw"),
    (isContact, "Contact")
  ]
  where
    isBullet = moveBullet dbMove
    isPulse = movePulse dbMove
    isDrain = moveDrain dbMove
    isBite = moveBite dbMove
    isPunch = movePunch dbMove
    isPowder = movePowder dbMove
    willCrit = moveWillCrit dbMove
    isSound = moveSound dbMove
    isDance = moveDance dbMove
    willDefrost = moveDefrost dbMove
    isContact = moveContact dbMove
    getList [] = []
    getList ((b, a):es) = if b then a : getList es else getList es

toAbility :: DBAbility -> Ability
toAbility dbAbility = Ability (abilityName dbAbility) (Just (abilityDesc dbAbility))

toItem :: DBItem -> Item
toItem dbItem = Item (itemName dbItem) (Just (itemDesc dbItem)) (isBerry dbItem) (flingBp dbItem) (onPlate dbItem) (onDrive dbItem) (onMemory dbItem)

toDTType :: DBData -> DTType
toDTType (DTI item) = DtItem (toItem item)
toDTType (DTA ability) = DtAbility (toAbility ability)
toDTType (DTM move) = DtMove (toMove move)
toDTType (DTP pokemon) = DtPokemon (toPokemon pokemon)

toEffectiveMove :: DBMove -> EffectiveMove
toEffectiveMove MoveT {..} = EM moveName moveBp movePriority(toDamageClass moveCategory) ((read . T.unpack . T.toLower)  moveType) moveContact (isJust moveSecondaryChance) moveUTO moveUDAO moveINO moveIPD moveIO moveID moveII moveIsMax moveBullet movePulse movePunch moveBite movePowder moveSound moveDance moveWillCrit 0 1
 where
   toDamageClass "Status" = OTHER
   toDamageClass "Physical" = PHYSICAL
   toDamageClass "Special" = SPECIAL
   toDamageClass other = OTHER