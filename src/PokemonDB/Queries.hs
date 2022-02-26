{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module PokemonDB.Queries
    ( getPokemon
    , getCompletePokemon
    , getItem
    , getAbility
    , getMove
    , getPokemonMoves
    , hasMove
    , hasMoves
    , getPokemonTypes
    , getPokemonAbilities
    , getMovesFrom
    , getPokemonsWithAbility
    , getPokemonsWithMove
    , getPokemonsWithStat
    , getCoverageMoves
    , getCategoryMoves
    , getCategoryCoverageMoves
    , getObjectClass
    , getData
    , TypeName
    , MoveName
    , AbilityName) where

import           Data.Maybe
import           Data.Text (Text, toTitle)
import           Opaleye hiding (not, null)
import qualified PokemonDB.Tables as T
import           PokemonDB.Types
import           Data.Profunctor.Product.Default
import           Database.PostgreSQL.Simple
import           Data.Either

selectPokemon :: PokemonId -> Select T.PokemonF
selectPokemon id = do
  pokemon <- T.selectPokemon
  isPokemon pokemon id
  return pokemon

isPokemon :: PokemonT b d n (Column SqlText) -> Text -> Select ()
isPokemon pokemon pId = where_ $ sqlStrictText pId .== pokemonId pokemon

getPokemon :: Connection -> PokemonId -> IO [DBPokemon]
getPokemon conn mon = runSelect conn $ selectPokemon mon

selectPokemonType :: PokemonId -> Select T.TypeF
selectPokemonType pokemon = do
  mon <- selectPokemon pokemon
  allTypeRs@(PTT pId tipe) <- T.selectPokemonType
  where_ $ pId .== pokemonId mon
  return (TypeT tipe)

getPokemonTypes :: Connection -> PokemonId -> IO [DBType]
getPokemonTypes con mon = runSelect con $ selectPokemonType mon

selectPokemonMoves :: PokemonId -> Select T.MoveF
selectPokemonMoves pokemon = do
  mon <- selectPokemon pokemon
  move <- T.selectMove
  pmr <- T.selectPokemonMove
  joinPokemonMove mon move pmr
  return move

joinPokemonMove :: T.PokemonF -> T.MoveF -> T.PokemonMoveF -> Select ()
joinPokemonMove pokemon move pmr = where_
  $ pokemonId pokemon .== pmPokemonId pmr .&& moveId move .== pmMoveId pmr

hasMoveSelect :: PokemonId -> MoveId -> Select T.PokemonF
hasMoveSelect pId mId = do
  pokemon <- T.selectPokemon
  move <- T.selectMove
  pmr <- T.selectPokemonMove
  isPokemon pokemon pId
  isMove move mId
  joinPokemonMove pokemon move pmr
  return pokemon

hasMove :: Connection -> PokemonId -> MoveId -> IO Bool
hasMove con mon move = not . null
  <$> (runSelect con $ hasMoveSelect mon move :: IO [DBPokemon])

hasMovesSelect :: PokemonId -> [MoveId] -> Select T.MoveF
hasMovesSelect pId mIds = do
  move <- selectPokemonMoves pId
  containsMove mIds move
  return move

containsMove :: [MoveId] -> T.MoveF -> Select ()
containsMove mIds move = where_ $ map sqlStrictText mIds `in_` moveId move

hasMoves :: Connection -> PokemonId -> [MoveId] -> IO Bool
hasMoves con pId mIds = (\moves -> length moves == length mIds)
  <$> (runSelect con $ hasMovesSelect pId mIds :: IO [DBMove])

getMovesFrom :: Connection -> PokemonId -> [MoveId] -> IO [DBMove]
getMovesFrom con pId mIds = runSelect con $ hasMovesSelect pId mIds

getPokemonMoves :: Connection -> PokemonId -> IO [DBMove]
getPokemonMoves con mon = runSelect con $ selectPokemonMoves mon

selectPokemonAbilities :: PokemonId -> Select T.AbilityF
selectPokemonAbilities pokemon = do
  mon <- selectPokemon pokemon
  ability <- T.selectAbility
  pma <- T.selectPokemonAbility
  where_
    $ pokemonId mon .== paPokemonId pma .&& paAbilityId pma
    .== abilityId ability
  pure ability

getPokemonAbilities :: Connection -> PokemonId -> IO [DBAbility]
getPokemonAbilities con mon = runSelect con $ selectPokemonAbilities mon

getCompletePokemon
  :: Connection -> PokemonId -> IO (Either Text DBCompletePokemon)
getCompletePokemon con mon = do
  pokemon <- getPokemon con mon
  types <- getPokemonTypes con mon
  abilities <- getPokemonAbilities con mon
  moves <- getPokemonMoves con mon
  let pl = length pokemon
      tl = length types
      al = length abilities
      ml = length moves
  if pl /= 1 || tl == 0 || al == 0
    then return (Left mon)
    else return
      $ Right
        (head pokemon, map typeName types, map abilityName abilities, moves)

selectPokemonWithMove :: MoveId -> Select T.PokemonF
selectPokemonWithMove mId = do
  mon <- T.selectPokemon
  move <- T.selectMove
  pmr <- T.selectPokemonMove
  joinPokemonMove mon move pmr
  isMove move mId
  return mon

getPokemonsWithMove :: Connection -> MoveId -> IO [DBPokemon]
getPokemonsWithMove con mId = runSelect con $ selectPokemonWithMove mId

selectPokemonWithAbility :: AbilityId -> Select T.PokemonF
selectPokemonWithAbility aId = do
  mon <- T.selectPokemon
  ability <- T.selectAbility
  par <- T.selectPokemonAbility
  joinPokemonAbility mon ability par
  isAbility ability aId
  return mon

joinPokemonAbility
  :: T.PokemonF -> T.AbilityF -> T.PokemonAbilityF -> Select ()
joinPokemonAbility mon ability par = where_
  $ pokemonId mon .== paPokemonId par .&& abilityId ability .== paAbilityId par

getPokemonsWithAbility :: Connection -> AbilityId -> IO [DBPokemon]
getPokemonsWithAbility con ability =
  runSelect con $ selectPokemonWithAbility ability

selectPokemonsWithStat
  :: (T.PokemonF -> Column SqlInt4) -> Int -> Select T.PokemonF
selectPokemonsWithStat f stat = do
  mon <- T.selectPokemon
  where_ $ f mon .== toFields stat
  return mon

getPokemonsWithStat :: Connection -> Stat -> Int -> IO (Maybe [DBPokemon])
getPokemonsWithStat con "atk" stat = Just
  <$> runSelect con (selectPokemonsWithStat pokemonAtk stat)
getPokemonsWithStat con "def" stat = Just
  <$> runSelect con (selectPokemonsWithStat pokemonDef stat)
getPokemonsWithStat con "spa" stat = Just
  <$> runSelect con (selectPokemonsWithStat pokemonSpa stat)
getPokemonsWithStat con "spd" stat = Just
  <$> runSelect con (selectPokemonsWithStat pokemonSpd stat)
getPokemonsWithStat con "spe" stat = Just
  <$> runSelect con (selectPokemonsWithStat pokemonSpe stat)
getPokemonsWithStat con "hp" stat = Just
  <$> runSelect con (selectPokemonsWithStat pokemonHp stat)
getPokemonsWithStat _ _ _ = return Nothing

selectMove :: MoveId -> Select T.MoveF
selectMove mId = do
  move <- T.selectMove
  isMove move mId
  return move

isMove :: T.MoveF -> MoveId -> Select ()
isMove move mId = where_ $ moveId move .== toFields mId

getMove :: Connection -> MoveId -> IO [DBMove]
getMove con move = runSelect con $ selectMove move

selectAbility :: AbilityId -> Select T.AbilityF
selectAbility aId = do
  ability <- T.selectAbility
  isAbility ability aId
  return ability

isAbility :: T.AbilityF -> AbilityId -> Select ()
isAbility ability aId = where_ $ sqlStrictText aId .== abilityId ability

getAbility :: Connection -> AbilityId -> IO [DBAbility]
getAbility con ability = runSelect con $ selectAbility ability

selectItem :: ItemId -> Select T.ItemF
selectItem iId = do
  item <- T.selectItem
  isItem item iId
  return item

isItem :: T.ItemF -> ItemId -> Select ()
isItem item iid = where_ $ itemId item .== sqlStrictText iid

getItem :: Connection -> ItemId -> IO [DBItem]
getItem con item = runSelect con $ selectItem item

selectCoverageMoves :: PokemonId -> MoveType -> Select T.MoveF
selectCoverageMoves pId tipe = do
  move <- selectPokemonMoves pId
  isMoveType move tipe
  return move

getCoverageMoves :: Connection -> PokemonId -> MoveType -> IO [DBMove]
getCoverageMoves con mon tipe = runSelect con $ selectCoverageMoves mon tipe

isMoveType :: T.MoveF -> MoveType -> Select ()
isMoveType move tipe = where_ $ moveType move .== toFields (toTitle tipe)

selectCategoryMoves :: PokemonId -> MoveCategory -> Select T.MoveF
selectCategoryMoves pId category = do
  move <- selectPokemonMoves pId
  isMoveCategory move category
  return move

isMoveCategory :: T.MoveF -> MoveCategory -> Select ()
isMoveCategory move category = where_
  $ moveCategory move .== toFields (toTitle category)

getCategoryMoves :: Connection -> PokemonId -> MoveCategory -> IO [DBMove]
getCategoryMoves con pId category =
  runSelect con $ selectCategoryMoves pId category

selectCategoryCoverageMoves
  :: PokemonId -> MoveCategory -> MoveType -> Select T.MoveF
selectCategoryCoverageMoves pId category coverage = do
  categoryMove <- selectCategoryMoves pId category
  isMoveType categoryMove coverage
  return categoryMove

getCategoryCoverageMoves
  :: Connection -> PokemonId -> MoveCategory -> MoveType -> IO [DBMove]
getCategoryCoverageMoves con pId category coverage =
  runSelect con $ selectCategoryCoverageMoves pId category coverage

selectObjectClass :: ObjectId -> Select T.OCF
selectObjectClass oId = do
  oc <- T.selectOC
  isOc oc oId
  return oc

isOc :: T.OCF -> ObjectId -> Select ()
isOc oc oId = where_ $ ocId oc .== toFields oId

getObjectClass :: Connection -> ObjectId -> IO (Maybe ObjectClass)
getObjectClass con oId = (\l -> if length l == 1
                                then Just (head l)
                                else Nothing)
  <$> runSelect con (selectObjectClass oId)

getData :: Connection -> ObjectId -> IO (Maybe DBData)
getData con oId = do
  oc <- getObjectClass con oId
  if isNothing oc
    then return Nothing
    else getDt con oId ((ocClass . fromJust) oc)

getDt :: Connection -> ObjectId -> Text -> IO (Maybe DBData)
getDt con oId "item" = Just . DTI . head <$> getItem con oId
getDt con oId "move" = Just . DTM . head <$> getMove con oId
getDt con oId "ability" = Just . DTA . head <$> getAbility con oId
getDt con oId "pokemon" = do
  mon <- getCompletePokemon con oId
  return
    $ if isLeft mon
      then Nothing
      else (Just . DTP . extractRight) mon
getDt con oId other = return Nothing

extractRight :: Either a p -> p
extractRight (Right a) = a
extractRight (Left _) = error "Tried extracting Right from Left"
