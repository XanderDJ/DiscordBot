module Pokemon.TypeMatchups (
    Matchups (..),
    AttackMap,
    DefenseMap,
    AttackRelation (..),
    DefenseRelation (..),
    TypeMatchup (..),
    matchups,
    getTypeMatchup,
    getMatchup,
    getAttackRelation,
    getDefenseRelation,
    getAttackMap,
    getAttackMap',
    getDefenseMap,
    getDefenseMap',
    combineAttackMap,
    combineDefenseMap,
    toMultiplier,
    removeImmunities,
    combineMatchups,
    getCombinedMatchup
) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Pokemon.Functions
import Pokemon.Types (Type (..))

matchups :: Matchups
matchups =
  M.fromList
    [ (FIRE, fireMatchup),
      (BUG, bugMatchup),
      (FIGHTING, fightingMatchup),
      (FLYING, flyingMatchup),
      (GROUND, groundMatchup),
      (ELECTRIC, electricMatchup),
      (STEEL, steelMatchup),
      (NORMAL, normalMatchup),
      (DARK, darkMatchup),
      (GHOST, ghostMatchup),
      (FAIRY, fairyMatchup),
      (PSYCHIC, psychicMatchup),
      (POISON, poisonMatchup),
      (ROCK, rockMatchup),
      (GRASS, grassMatchup),
      (WATER, waterMatchup),
      (ICE, iceMatchup),
      (DRAGON, dragonMatchup)
    ]

type Matchups = M.Map Type TypeMatchup

type AttackMap = M.Map Type AttackRelation

type DefenseMap = M.Map Type DefenseRelation

data AttackRelation = Neutral | Immune | SuperEffective | StronglyEffective | Resisted | StronglyResisted deriving (Show, Eq)

instance Semigroup AttackRelation where
  (<>) Neutral r = r
  (<>) r Neutral = r
  (<>) Immune r = Immune
  (<>) r Immune = Immune
  (<>) SuperEffective StronglyResisted = Resisted
  (<>) StronglyResisted SuperEffective = Resisted
  (<>) SuperEffective StronglyEffective = StronglyEffective
  (<>) StronglyEffective SuperEffective = StronglyEffective
  (<>) Resisted StronglyResisted = StronglyResisted
  (<>) StronglyResisted Resisted = StronglyResisted
  (<>) StronglyEffective Resisted = SuperEffective
  (<>) Resisted StronglyEffective = SuperEffective
  (<>) Resisted Resisted = StronglyResisted
  (<>) SuperEffective SuperEffective = StronglyEffective
  (<>) _ _ = Neutral

instance Monoid AttackRelation where
  mempty = Neutral

type DefenseRelation = AttackRelation

data TypeMatchup = TM
  { attackM :: AttackMap,
    defenseM :: DefenseMap
  }
  deriving (Eq, Show)

instance Semigroup TypeMatchup where
  (<>) = combineMatchups

instance Monoid TypeMatchup where
  mempty = TM mempty mempty

fireAttack :: AttackMap
fireAttack =
  M.fromList
    [ (WATER, Resisted),
      (ROCK, Resisted),
      (DRAGON, Resisted),
      (FIRE, Resisted),
      (BUG, SuperEffective),
      (GRASS, SuperEffective),
      (ICE, SuperEffective),
      (STEEL, SuperEffective)
    ]

fireDefense :: DefenseMap
fireDefense =
  M.fromList
    [ (BUG, Resisted),
      (FAIRY, Resisted),
      (FIRE, Resisted),
      (GRASS, Resisted),
      (ICE, Resisted),
      (STEEL, Resisted),
      (GROUND, SuperEffective),
      (ROCK, SuperEffective),
      (WATER, SuperEffective)
    ]

fireMatchup :: TypeMatchup
fireMatchup = TM fireAttack fireDefense

bugAttack :: AttackMap
bugAttack =
  M.fromList
    [ (DARK, SuperEffective),
      (GRASS, SuperEffective),
      (PSYCHIC, SuperEffective),
      (FAIRY, Resisted),
      (FIGHTING, Resisted),
      (FIRE, Resisted),
      (FLYING, Resisted),
      (GHOST, Resisted),
      (POISON, Resisted),
      (STEEL, Resisted)
    ]

bugDefense :: DefenseMap
bugDefense =
  M.fromList
    [ (FIGHTING, Resisted),
      (GRASS, Resisted),
      (GROUND, Resisted),
      (FIRE, SuperEffective),
      (FLYING, SuperEffective),
      (ROCK, SuperEffective)
    ]

bugMatchup = TM bugAttack bugDefense

fightingAttack =
  M.fromList
    [ (GHOST, Immune),
      (BUG, Resisted),
      (FAIRY, Resisted),
      (FLYING, Resisted),
      (POISON, Resisted),
      (PSYCHIC, Resisted),
      (DARK, SuperEffective),
      (ICE, SuperEffective),
      (NORMAL, SuperEffective),
      (ROCK, SuperEffective),
      (STEEL, SuperEffective)
    ]

figthingDefense =
  M.fromList
    [ (BUG, Resisted),
      (DARK, Resisted),
      (ROCK, Resisted),
      (FAIRY, SuperEffective),
      (FLYING, SuperEffective),
      (PSYCHIC, SuperEffective)
    ]

fightingMatchup = TM fightingAttack figthingDefense

flyingAttack =
  M.fromList
    [ (ELECTRIC, Resisted),
      (ROCK, Resisted),
      (STEEL, Resisted),
      (BUG, SuperEffective),
      (FIGHTING, SuperEffective),
      (GRASS, SuperEffective)
    ]

flyingDefense =
  M.fromList
    [ (GROUND, Immune),
      (BUG, Resisted),
      (FIGHTING, Resisted),
      (GRASS, Resisted),
      (ELECTRIC, SuperEffective),
      (ICE, SuperEffective),
      (ROCK, SuperEffective)
    ]

flyingMatchup = TM flyingAttack flyingDefense

groundAttack =
  M.fromList
    [ (FLYING, Immune),
      (BUG, Resisted),
      (GRASS, Resisted),
      (ELECTRIC, SuperEffective),
      (FIRE, SuperEffective),
      (POISON, SuperEffective),
      (ROCK, SuperEffective),
      (STEEL, SuperEffective)
    ]

groundDefense =
  M.fromList
    [ (ELECTRIC, Immune),
      (POISON, Resisted),
      (ROCK, Resisted),
      (GRASS, SuperEffective),
      (ICE, SuperEffective),
      (WATER, SuperEffective)
    ]

groundMatchup = TM groundAttack groundDefense

electricAttack =
  M.fromList
    [ (GROUND, Immune),
      (DRAGON, Resisted),
      (ELECTRIC, Resisted),
      (GRASS, Resisted),
      (FLYING, SuperEffective),
      (WATER, SuperEffective)
    ]

electricDefense =
  M.fromList
    [ (ELECTRIC, Resisted),
      (FLYING, Resisted),
      (STEEL, Resisted),
      (GROUND, SuperEffective)
    ]

electricMatchup = TM electricAttack electricDefense

steelAttack =
  M.fromList
    [ (ELECTRIC, Resisted),
      (FIRE, Resisted),
      (STEEL, Resisted),
      (WATER, Resisted),
      (FAIRY, SuperEffective),
      (ICE, SuperEffective),
      (ROCK, SuperEffective)
    ]

steelDefense =
  M.fromList
    [ (POISON, Immune),
      (BUG, Resisted),
      (DRAGON, Resisted),
      (FAIRY, Resisted),
      (FLYING, Resisted),
      (GRASS, Resisted),
      (ICE, Resisted),
      (NORMAL, Resisted),
      (PSYCHIC, Resisted),
      (ROCK, Resisted),
      (STEEL, Resisted),
      (FIRE, SuperEffective),
      (GROUND, SuperEffective)
    ]

steelMatchup = TM steelAttack steelDefense

normalAttack =
  M.fromList
    [ (GHOST, Immune),
      (ROCK, Resisted),
      (STEEL, Resisted)
    ]

normalDefense =
  M.fromList
    [ (GHOST, Immune),
      (FIGHTING, SuperEffective)
    ]

normalMatchup = TM normalAttack normalDefense

iceAttack =
  M.fromList
    [ (FIRE, Resisted),
      (ICE, Resisted),
      (STEEL, Resisted),
      (WATER, Resisted),
      (DRAGON, SuperEffective),
      (FLYING, SuperEffective),
      (GRASS, SuperEffective),
      (GROUND, SuperEffective)
    ]

iceDefense =
  M.fromList
    [ (ICE, Resisted),
      (FIGHTING, SuperEffective),
      (FIRE, SuperEffective),
      (ROCK, SuperEffective),
      (STEEL, SuperEffective)
    ]

iceMatchup = TM iceAttack iceDefense

poisonAttack =
  M.fromList
    [ (STEEL, Immune),
      (GHOST, Resisted),
      (GROUND, Resisted),
      (POISON, Resisted),
      (ROCK, Resisted),
      (FAIRY, SuperEffective),
      (GRASS, SuperEffective)
    ]

poisonDefense =
  M.fromList
    [ (BUG, Resisted),
      (FAIRY, Resisted),
      (FIGHTING, Resisted),
      (GRASS, Resisted),
      (POISON, Resisted),
      (GROUND, SuperEffective),
      (PSYCHIC, SuperEffective)
    ]

poisonMatchup = TM poisonAttack poisonDefense

psychicAttack =
  M.fromList
    [ (DARK, Immune),
      (PSYCHIC, Resisted),
      (STEEL, Resisted),
      (FIGHTING, SuperEffective),
      (POISON, SuperEffective)
    ]

psychicDefense =
  M.fromList
    [ (FIGHTING, Resisted),
      (PSYCHIC, Resisted),
      (BUG, SuperEffective),
      (DARK, SuperEffective),
      (GHOST, SuperEffective)
    ]

psychicMatchup = TM psychicAttack psychicDefense

darkAttack =
  M.fromList
    [ (DARK, Resisted),
      (FAIRY, Resisted),
      (FIGHTING, Resisted),
      (GHOST, SuperEffective),
      (PSYCHIC, SuperEffective)
    ]

darkDefense =
  M.fromList
    [ (PSYCHIC, Immune),
      (DARK, Resisted),
      (GHOST, Resisted),
      (BUG, SuperEffective),
      (FAIRY, SuperEffective),
      (FIGHTING, SuperEffective)
    ]

darkMatchup = TM darkAttack darkDefense

ghostAttack =
  M.fromList
    [ (NORMAL, Immune),
      (DARK, Resisted),
      (GHOST, SuperEffective),
      (PSYCHIC, SuperEffective)
    ]

ghostDefense =
  M.fromList
    [ (FIGHTING, Immune),
      (NORMAL, Immune),
      (BUG, Resisted),
      (POISON, Resisted),
      (DARK, SuperEffective),
      (GHOST, SuperEffective)
    ]

ghostMatchup = TM ghostAttack ghostDefense

fairyAttack =
  M.fromList
    [ (FIRE, Resisted),
      (POISON, Resisted),
      (STEEL, Resisted),
      (DARK, SuperEffective),
      (DRAGON, SuperEffective),
      (FIGHTING, SuperEffective)
    ]

fairyDefense =
  M.fromList
    [ (DRAGON, Immune),
      (BUG, Resisted),
      (DARK, Resisted),
      (FIGHTING, Resisted),
      (POISON, SuperEffective),
      (STEEL, SuperEffective)
    ]

fairyMatchup = TM fairyAttack fairyDefense

rockAttack =
  M.fromList
    [ (FIGHTING, Resisted),
      (GROUND, Resisted),
      (STEEL, Resisted),
      (BUG, SuperEffective),
      (FIRE, SuperEffective),
      (FLYING, SuperEffective),
      (ICE, SuperEffective)
    ]

rockDefense =
  M.fromList
    [ (FIRE, Resisted),
      (FLYING, Resisted),
      (NORMAL, Resisted),
      (POISON, Resisted),
      (FIGHTING, SuperEffective),
      (GRASS, SuperEffective),
      (GROUND, SuperEffective),
      (STEEL, SuperEffective),
      (WATER, SuperEffective)
    ]

rockMatchup = TM rockAttack rockDefense

grassAttack =
  M.fromList
    [ (BUG, Resisted),
      (DRAGON, Resisted),
      (FIRE, Resisted),
      (FLYING, Resisted),
      (GRASS, Resisted),
      (POISON, Resisted),
      (STEEL, Resisted),
      (GROUND, SuperEffective),
      (ROCK, SuperEffective),
      (WATER, SuperEffective)
    ]

grassDefense =
  M.fromList
    [ (ELECTRIC, Resisted),
      (GRASS, Resisted),
      (GROUND, Resisted),
      (WATER, Resisted),
      (BUG, SuperEffective),
      (FIRE, SuperEffective),
      (FLYING, SuperEffective),
      (ICE, SuperEffective),
      (POISON, SuperEffective)
    ]

grassMatchup = TM grassAttack grassDefense

waterAttack =
  M.fromList
    [ (DRAGON, Resisted),
      (GRASS, Resisted),
      (WATER, Resisted),
      (FIRE, SuperEffective),
      (GROUND, SuperEffective),
      (ROCK, SuperEffective)
    ]

waterDefense =
  M.fromList
    [ (FIRE, Resisted),
      (ICE, Resisted),
      (STEEL, Resisted),
      (WATER, Resisted),
      (ELECTRIC, SuperEffective),
      (GRASS, SuperEffective)
    ]

waterMatchup = TM waterAttack waterDefense

dragonAttack =
  M.fromList
    [ (FAIRY, Immune),
      (STEEL, Resisted),
      (DRAGON, SuperEffective)
    ]

dragonDefense =
  M.fromList
    [ (ELECTRIC, Resisted),
      (FIRE, Resisted),
      (GRASS, Resisted),
      (WATER, Resisted),
      (DRAGON, SuperEffective),
      (FAIRY, SuperEffective),
      (ICE, SuperEffective)
    ]

dragonMatchup = TM dragonAttack dragonDefense

getMatchup :: Type -> TypeMatchup
getMatchup = (M.!) matchups

combineMatchups :: TypeMatchup -> TypeMatchup -> TypeMatchup
combineMatchups (TM am dm) (TM am' dm') = TM (combineAttackMap am am') (combineDefenseMap dm dm')

getCombinedMatchup :: [Type] -> Maybe TypeMatchup
getCombinedMatchup [] = Nothing
getCombinedMatchup ts = Just $ foldMap getMatchup ts

getAttackMap :: [Type] -> AttackMap
getAttackMap = foldl1 combineAttackMap . map (attackM . getMatchup) . L.nub

getAttackMap' :: Type -> AttackMap
getAttackMap' = attackM . getMatchup 

getDefenseMap :: [Type] -> DefenseMap
getDefenseMap = foldl1 combineDefenseMap . map (defenseM . getMatchup) . L.nub

getDefenseMap' :: Type -> DefenseMap
getDefenseMap' = defenseM . getMatchup

getAttackRelation :: AttackMap -> Type -> AttackRelation
getAttackRelation am t = fromMaybe Neutral (am M.!? t)

getDefenseRelation :: DefenseMap -> Type -> DefenseRelation
getDefenseRelation = getAttackRelation

combineAttackMap :: AttackMap -> AttackMap -> AttackMap
combineAttackMap = M.unionWith (<>)

combineDefenseMap :: DefenseMap -> DefenseMap -> DefenseMap
combineDefenseMap = combineAttackMap

getTypeMatchup :: [Type] -> [Type] -> AttackRelation
getTypeMatchup attackTypes defenseType = foldl1 (<>) relations
  where
    am = getAttackMap attackTypes
    relations = map (getAttackRelation am) defenseType

toMultiplier :: Fractional p => AttackRelation -> p
toMultiplier Neutral = 1
toMultiplier Immune = 0
toMultiplier SuperEffective = 2
toMultiplier Resisted = 0.5
toMultiplier StronglyResisted = 0.25
toMultiplier StronglyEffective = 4

removeImmunities :: TypeMatchup -> TypeMatchup
removeImmunities (TM am dm) = TM am' dm'
 where
   am' = foldl (flip $ M.update filterImmunity) am (M.keys am)
   dm' = foldl (flip $ M.update filterImmunity) dm (M.keys dm)


filterImmunity :: AttackRelation -> Maybe AttackRelation
filterImmunity Immune = Nothing
filterImmunity ar = Just ar