{-# LANGUAGE GADTs #-}

module ChanceCalculator.ChanceList (ChanceList(..), insertChance, getChance, validateChanceList, fixChanceList, fromList, toList, reduceCL) where

import qualified Data.List as L (sortOn)
import Data.Monoid

-- | List that contains the chance of an event occuring after a specific amount of tries. The tries in the chance list should be in ascending order.
data ChanceList n c where
  CLEmpty :: Ord n => ChanceList n c
  CLNode :: Ord n => n -> c -> ChanceList n c -> ChanceList n c

-- | Insert a try chance combination into the chance list.
--   If there is already an chance at try n then the chance will be updated. Otherwise it will be inserted correctly.
insertChance :: Ord n => ChanceList n c -> n -> c -> ChanceList n c
insertChance CLEmpty n c = CLNode n c CLEmpty
insertChance (CLNode n' c' oldChanceList) n c
  | n > n' = CLNode n' c' (insertChance oldChanceList n c)
  | n == n' = CLNode n c oldChanceList
  | otherwise = CLNode n c (CLNode n' c' oldChanceList)

-- | Transform a list to a ChanceList.
fromList :: Ord n => [(n, c)] -> ChanceList n c
fromList = foldl insertChance' CLEmpty
  where
    insertChance' cl (n, c) = insertChance cl n c

-- | Check if the given ChanceList is valid. All chances should be ordered at ascending tries.
validateChanceList :: Ord n => ChanceList n c -> Bool
validateChanceList CLEmpty = True
validateChanceList (CLNode _ _ CLEmpty) = True
validateChanceList (CLNode n c (CLNode n' c' cl')) = n < n' && validateChanceList cl'

-- | Fix an invalid  ChanceList. It will first validate that the
fixChanceList :: Ord n => ChanceList n c -> ChanceList n c
fixChanceList cl = fromList (L.sortOn fst tcs)
  where
    tcs = toList cl

-- | get Chance c at try n from chancelist. If n is smaller than the first n found then return default parameter.
--   Also returns default parameter when Empty chancelist is given.
getChance :: Ord n => ChanceList n c -> n -> c -> c
getChance CLEmpty n c = c
getChance (CLNode n' c' cl) n c = if n < n' then c else getChance cl n c'

-- | reduce ChanceList to only the tries Just before and after the given try.
reduceCL :: Ord n => ChanceList n c -> n -> ChanceList n c
reduceCL CLEmpty _ = CLEmpty
reduceCL cl@(CLNode n _ CLEmpty) _ = cl
reduceCL cl@(CLNode n' _ cl'@(CLNode n'' _ _)) n
  | n < n' = cl
  | n < n'' = cl
  | otherwise = reduceCL cl' n

-- | getList from ChanceList. o(n)
toList :: ChanceList n c -> [(n, c)]
toList CLEmpty = []
toList (CLNode n c cl) = (n, c) : toList cl

instance Foldable (ChanceList n) where
  foldMap f CLEmpty = mempty
  foldMap f (CLNode _ c cl) = f c <> foldMap f cl

instance (Show n, Show c) => Show (ChanceList n c) where
  show CLEmpty = "CLEmpty"
  show (CLNode n c cl) = "CLNode " ++ show n ++ " " ++ show c ++ " (" ++ show cl ++ ")"