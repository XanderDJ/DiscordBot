{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module PokemonDB.Queries
  ( getPokemon,
    getCompletePokemon,
    getItem,
    getAbility,
    getMove,
    getPokemonMoves,
    hasMove,
    hasMoves,
    getPokemonTypes,
    getPokemonAbilities,
    getMovesFrom,
    getPokemonsWithAbility,
    getPokemonsWithMove,
    getPokemonsWithStat,
    getCoverageMoves,
    getCategoryMoves,
    getCategoryCoverageMoves,
    getObjectClass,
    getData,
    runPokemonQuery,
  )
where

import Data.Either
import qualified Data.List as L
import Data.Maybe
import Data.Profunctor.Product.Default
import Data.Text (Text, toTitle)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Opaleye hiding (not, null)
import qualified PokemonDB.Tables as T
import PokemonDB.Types

runPokemonQuery :: Connection -> PokemonQuery -> IO PokemonQueryResult
runPokemonQuery con (Learn pId mIds) = LearnR <$> getMovesFrom con (toId pId) (map toId mIds)
runPokemonQuery con (DT oId) = DTR <$> getData con (toId oId)
runPokemonQuery con (AllMoves pId) = AllMovesR <$> getPokemonMoves con (toId pId)
runPokemonQuery con (AllMovesFromType pId tipe) = AllMovesFromTypeR <$> getCoverageMoves con (toId pId) (toTitle tipe)
runPokemonQuery con (AllMovesFromCategory pId category) = AllMovesFromCategoryR <$> getCategoryMoves con (toId pId) category
runPokemonQuery con (AllMovesFromCategoryAndType pId category tipe) = AllMovesFromCategoryAndTypeR <$> getCategoryCoverageMoves con (toId pId) category (toTitle tipe)
runPokemonQuery con (AllPokemonWithStat stat value) = AllPokemonWithStatR <$> do getPokemonsWithStat con stat value
runPokemonQuery con (AllPokemonsWithAbility aId) = AllPokemonsWithAbilityR <$> getPokemonsWithAbility con (toId aId)
runPokemonQuery con (AllPokemonsWithMove mId) = AllPokemonsWithMoveR <$> do getPokemonsWithMove con (toId mId)
runPokemonQuery con (AllPriorityMoves pId) = AllPriorityMovesR <$> getPriorityMoves con (toId pId)
runPokemonQuery con (AllHazardMoves pId) = AllHazardMovesR <$> getHazardMoves con (toId pId)
runPokemonQuery con (AllClericMoves pId) = AllClericMovesR <$> getClericMoves con (toId pId)
runPokemonQuery con (AllRecoveryMoves pId) = AllRecoveryMovesR <$> getRecoveryMoves con (toId pId)
runPokemonQuery con (AllScreens pId) = AllScreensR <$> getScreenMoves con (toId pId)
runPokemonQuery con (AllHazardControl pId) = AllHazardControlR <$> getHazardControlMoves con (toId pId)
runPokemonQuery con (AllSetUpMoves pId) = AllSetUpMovesR <$> getSetUpMoves con (toId pId)
runPokemonQuery con (AllStatusMoves pId) = AllStatusMovesR <$> getStatusMoves con (toId pId)

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

joinPokemonType :: T.PokemonF -> T.TypeF -> T.PokemonTypeF -> Select ()
joinPokemonType mon tipe ptr = where_ $ pokemonId mon .== ptPokemonId ptr .&& typeName tipe .== ptType ptr

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
joinPokemonMove pokemon move pmr =
  where_ $
    pokemonId pokemon .== pmPokemonId pmr .&& moveId move .== pmMoveId pmr

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
hasMove con mon move =
  not . null
    <$> (runSelect con $ hasMoveSelect mon move :: IO [DBPokemon])

hasMovesSelect :: PokemonId -> [MoveId] -> Select T.MoveF
hasMovesSelect pId mIds = do
  move <- selectPokemonMoves pId
  containsMove mIds move
  return move

containsMove :: [MoveId] -> T.MoveF -> Select ()
containsMove mIds move = where_ $ map sqlStrictText mIds `in_` moveId move

hasMoves :: Connection -> PokemonId -> [MoveId] -> IO Bool
hasMoves con pId mIds =
  (\moves -> length moves == length mIds)
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
  where_ $
    pokemonId mon .== paPokemonId pma .&& paAbilityId pma
      .== abilityId ability
  pure ability

getPokemonAbilities :: Connection -> PokemonId -> IO [DBAbility]
getPokemonAbilities con mon = runSelect con $ selectPokemonAbilities mon

getCompletePokemon ::
  Connection -> PokemonId -> IO (Either Text DBCompletePokemon)
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
    else
      return $
        Right
          (head pokemon, map typeName types, map abilityName abilities, moves)

selectPokemonWithMove :: MoveId -> Select (T.PokemonF, T.TypeF, T.AbilityF, T.MoveF)
selectPokemonWithMove mId = do
  mon <- T.selectPokemon
  move <- T.selectMove
  pmr <- T.selectPokemonMove
  tipe <- T.selectType
  ptr <- T.selectPokemonType
  ability <- T.selectAbility
  par <- T.selectPokemonAbility
  joinPokemonAbility mon ability par
  joinPokemonType mon tipe ptr
  joinPokemonMove mon move pmr
  isMove move mId
  return (mon, tipe, ability, move)

getPokemonsWithMove :: Connection -> MoveId -> IO [DBCompletePokemon]
getPokemonsWithMove con mId = do
  result <- runSelect con $ selectPokemonWithMove mId
  if null result then return [] else return $ getCompletePokemonFromResult result

selectPokemonWithAbility :: AbilityId -> Select (T.PokemonF, T.TypeF, T.AbilityF, T.MoveF)
selectPokemonWithAbility aId = do
  mon <- T.selectPokemon
  pmr <- T.selectPokemonMove
  move <- T.selectMove
  ability <- T.selectAbility
  par <- T.selectPokemonAbility
  ptr <- T.selectPokemonType
  tipe <- T.selectType
  joinPokemonMove mon move pmr
  joinPokemonType mon tipe ptr
  joinPokemonAbility mon ability par
  isAbility ability aId
  return (mon, tipe, ability, move)

joinPokemonAbility ::
  T.PokemonF -> T.AbilityF -> T.PokemonAbilityF -> Select ()
joinPokemonAbility mon ability par =
  where_ $
    pokemonId mon .== paPokemonId par .&& abilityId ability .== paAbilityId par

getPokemonsWithAbility :: Connection -> AbilityId -> IO [DBCompletePokemon]
getPokemonsWithAbility con ability = do
  result <- runSelect con $ selectPokemonWithAbility ability
  if L.null result
    then return []
    else do return $ getCompletePokemonFromResult result

getCompletePokemonFromResult :: [(DBPokemon, DBType, DBAbility, DBMove)] -> [DBCompletePokemon]
getCompletePokemonFromResult result =
  let grouped = L.groupBy groupFunction result
   in map toCompletePokemon grouped
  where
    groupFunction (a, b, c, d) (a', b', c', d') = a == a'
    toCompletePokemon :: [(DBPokemon, DBType, DBAbility, DBMove)] -> DBCompletePokemon
    toCompletePokemon l =
      let (mons, types, abilities, moves) = (map fst4 l, map (typeName . snd4) l, map (abilityName . third4) l, map last4 l)
       in (head mons, L.nub types, L.nub abilities, L.nub moves)

selectPokemonsWithStat ::
  (T.PokemonF -> Column SqlInt4) -> Int -> Select (T.PokemonF, T.TypeF, T.AbilityF, T.MoveF)
selectPokemonsWithStat f stat = do
  mon <- T.selectPokemon
  pmr <- T.selectPokemonMove
  move <- T.selectMove
  ability <- T.selectAbility
  par <- T.selectPokemonAbility
  ptr <- T.selectPokemonType
  tipe <- T.selectType
  joinPokemonMove mon move pmr
  joinPokemonType mon tipe ptr
  joinPokemonAbility mon ability par
  where_ $ f mon .== toFields stat
  return (mon, tipe, ability, move)

getPokemonsWithStat :: Connection -> Stat -> Int -> IO [DBCompletePokemon]
getPokemonsWithStat con "atk" stat = runSelect con (selectPokemonsWithStat pokemonAtk stat) >>= toCompletePokemon
getPokemonsWithStat con "attack" stat = runSelect con (selectPokemonsWithStat pokemonAtk stat) >>= toCompletePokemon
getPokemonsWithStat con "def" stat = runSelect con (selectPokemonsWithStat pokemonDef stat) >>= toCompletePokemon
getPokemonsWithStat con "defense" stat = runSelect con (selectPokemonsWithStat pokemonDef stat) >>= toCompletePokemon
getPokemonsWithStat con "spa" stat = runSelect con (selectPokemonsWithStat pokemonSpa stat) >>= toCompletePokemon
getPokemonsWithStat con "specialattack" stat = runSelect con (selectPokemonsWithStat pokemonSpa stat) >>= toCompletePokemon
getPokemonsWithStat con "spd" stat = runSelect con (selectPokemonsWithStat pokemonSpd stat) >>= toCompletePokemon
getPokemonsWithStat con "specialdefense" stat = runSelect con (selectPokemonsWithStat pokemonSpd stat) >>= toCompletePokemon
getPokemonsWithStat con "spe" stat = runSelect con (selectPokemonsWithStat pokemonSpe stat) >>= toCompletePokemon
getPokemonsWithStat con "speed" stat = runSelect con (selectPokemonsWithStat pokemonSpe stat) >>= toCompletePokemon
getPokemonsWithStat con "hp" stat = runSelect con (selectPokemonsWithStat pokemonHp stat) >>= toCompletePokemon
getPokemonsWithStat _ _ _ = pure []

toCompletePokemon :: Monad m => [(DBPokemon, DBType, DBAbility, DBMove)] -> m [DBCompletePokemon]
toCompletePokemon r = if null r then return [] else return $ getCompletePokemonFromResult r

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
isMoveCategory move category =
  where_ $
    moveCategory move .== toFields (toTitle category)

getCategoryMoves :: Connection -> PokemonId -> MoveCategory -> IO [DBMove]
getCategoryMoves con pId category =
  runSelect con $ selectCategoryMoves pId category

selectCategoryCoverageMoves ::
  PokemonId -> MoveCategory -> MoveType -> Select T.MoveF
selectCategoryCoverageMoves pId category coverage = do
  categoryMove <- selectCategoryMoves pId category
  isMoveType categoryMove coverage
  return categoryMove

getCategoryCoverageMoves ::
  Connection -> PokemonId -> MoveCategory -> MoveType -> IO [DBMove]
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
getObjectClass con oId =
  ( \l ->
      if length l == 1
        then Just (head l)
        else Nothing
  )
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
  return $
    if isLeft mon
      then Nothing
      else (Just . DTP . extractRight) mon
getDt con oId other = return Nothing

moveElemOf :: (Functor f, Foldable f) => f (Column a) -> MoveT b n (Column a) mn mf mt -> Select ()
moveElemOf moves move = where_ $ in_ moves (moveId move)

selectSpecificMoves :: (T.MoveF -> Select ()) -> PokemonId -> Select T.MoveF
selectSpecificMoves meetsCriteria pId = do
  move <- selectPokemonMoves pId
  meetsCriteria move
  return move

getSpecificMoves :: (PokemonId -> Select T.MoveF) -> Connection -> PokemonId -> IO [DBMove]
getSpecificMoves selectFunction con pId = runSelect con $ selectFunction pId

selectPriorityMoves :: PokemonId -> Select T.MoveF
selectPriorityMoves = selectSpecificMoves isPriority

getPriorityMoves :: Connection -> PokemonId -> IO [DBMove]
getPriorityMoves = getSpecificMoves selectPriorityMoves

isPriority :: T.MoveF -> Select ()
isPriority move = where_ $ movePriority move .> 0

selectHazardMoves :: PokemonId -> Select T.MoveF
selectHazardMoves = selectSpecificMoves isHazard

isHazard :: T.MoveF -> Select ()
isHazard = moveElemOf ["spikes", "toxicspikes", "stealthrock", "stickyweb"]

getHazardMoves :: Connection -> PokemonId -> IO [DBMove]
getHazardMoves = getSpecificMoves selectHazardMoves

selectScreenMoves :: PokemonId -> Select T.MoveF
selectScreenMoves = selectSpecificMoves isScreen

isScreen :: T.MoveF -> Select ()
isScreen = moveElemOf ["lightscreen", "reflect", "auroraveil"]

getScreenMoves :: Connection -> PokemonId -> IO [DBMove]
getScreenMoves = getSpecificMoves selectScreenMoves

selectClericMoves :: PokemonId -> Select T.MoveF
selectClericMoves = selectSpecificMoves isCleric

isCleric :: T.MoveF -> Select ()
isCleric = moveElemOf ["wish", "healbell", "aromatherapy", "healingwish", "lunardance"]

getClericMoves :: Connection -> PokemonId -> IO [DBMove]
getClericMoves = getSpecificMoves selectClericMoves

selectRecoveryMoves :: PokemonId -> Select T.MoveF
selectRecoveryMoves = selectSpecificMoves isRecovery

isRecovery :: T.MoveF -> Select ()
isRecovery =
  moveElemOf
    [ "moonlight",
      "morningsun",
      "roost",
      "milkdrink",
      "softboiled",
      "wish",
      "shoreup",
      "synthesis",
      "rest",
      "leechlife",
      "absorb",
      "megadrain",
      "gigadrain",
      "drainingkiss",
      "drainpunch",
      "painsplit",
      "leechseed",
      "junglehealing",
      "floralhealing",
      "lunardance",
      "lifedew",
      "aquaring",
      "healingwish",
      "healpulse",
      "pollenpuff",
      "ingrain",
      "present",
      "slackoff",
      "strengthsap",
      "oblivionwing",
      "swallow",
      "recover",
      "shoreup"
    ]

getRecoveryMoves :: Connection -> PokemonId -> IO [DBMove]
getRecoveryMoves = getSpecificMoves selectRecoveryMoves

selectHazardControlMoves :: PokemonId -> Select T.MoveF
selectHazardControlMoves = selectSpecificMoves isHazardControl

isHazardControl :: T.MoveF -> Select ()
isHazardControl = moveElemOf ["defog", "rapidspin", "courtchange"]

getHazardControlMoves :: Connection -> PokemonId -> IO [DBMove]
getHazardControlMoves = getSpecificMoves selectHazardControlMoves

selectSetUpMoves :: PokemonId -> Select T.MoveF
selectSetUpMoves = selectSpecificMoves isSetUp

isSetUp :: T.MoveF -> Select ()
isSetUp =
  moveElemOf
    [ "acidarmor",
      "acupressure",
      "agility",
      "amnesia",
      "autotomize",
      "bellydrum",
      "bulkup",
      "calmmind",
      "charge",
      "clangoroussoul",
      "coil",
      "coaching",
      "cosmicpower",
      "cottonguard",
      "defendorder",
      "defensecurl",
      "doubleteam",
      "dragondance",
      "focusenergy",
      "geomancy",
      "growth",
      "harden",
      "honeclaws",
      "howl",
      "irondefense",
      "nastyplot",
      "noretreat",
      "quiverdance",
      "rockpolish",
      "sharpen",
      "rototiller",
      "shellsmash",
      "shiftgear",
      "stuffcheeks",
      "swordsdance",
      "withdraw",
      "workup"
    ]

getSetUpMoves :: Connection -> PokemonId -> IO [DBMove]
getSetUpMoves = getSpecificMoves selectSetUpMoves

selectStatusMoves :: PokemonId -> Select T.MoveF
selectStatusMoves = selectSpecificMoves isStatus

isStatus :: T.MoveF -> Select ()
isStatus =
  moveElemOf
    [ "toxic",
      "willowisp",
      "thunderwave",
      "hypnosis",
      "glare",
      "sleeppowder",
      "stunspore",
      "darkvoid",
      "yawn",
      "confuseray",
      "swagger",
      "flatter",
      "poisonpowder",
      "poisongas",
      "sing",
      "sweetkiss",
      "teeterdance"
    ]

getStatusMoves :: Connection -> PokemonId -> IO [DBMove]
getStatusMoves = getSpecificMoves selectStatusMoves

extractRight :: Show a => Either a p -> p
extractRight e = case e of
  Left a -> error ("Tried to extract right from Left " ++ show a)
  Right p -> p

toId :: T.Text -> T.Text
toId = T.replace " " "" . T.replace "-" "" . T.toLower

printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . fromMaybe "Empty select" . showSql

fst4 :: (a, b, c, d) -> a
fst4 (a, b, c, d) = a

snd4 :: (a, b, c, d) -> b
snd4 (a, b, c, d) = b

third4 :: (a, b, c, d) -> c
third4 (a, b, c, d) = c

last4 :: (a, b, c, d) -> d
last4 (a, b, c, d) = d