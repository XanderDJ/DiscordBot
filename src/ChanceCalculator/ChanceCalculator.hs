{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module ChanceCalculator.ChanceCalculator where

import           ChanceCalculator.ChanceList    ( ChanceList
                                                , getChance
                                                , reduceCL
                                                )


-- | given a chance of an event occuring p, returns the amount of tries needed to get a desiredSuccessrate of the event to occur at least once
calculateTries :: (Floating a, RealFrac a) => a -> a -> Int
calculateTries p desiredSuccessRate =
  ceiling $ logBase (1 - p) (1 - desiredSuccessRate)

-- | given the amount of tries tried and the chance of the event occurring, calculates the chance of the event having occured at least once
chanceFor :: Floating a => a -> Int -> a
chanceFor p tries = 1 - (1 - p) ** fromIntegral tries

-- | Given a ChanceList that represents the chance of the event occurring at each n tries and the desired successrate of an event occurring at least once,
--   returns the tries needed to reach the desired successrate.
calculateTriesWithCL :: (Num c, Ord c) => ChanceList Int c -> c -> Int
calculateTriesWithCL cl dsr = go cl dsr cpf 0
 where
    -- current chance of failure
  cpf = (1 -) $ getChance cl 0 0
  -- Method to calculate current failure chance (old failure chance times new chance of failure)
  calculateCPF cl try = (*) (1 - getChance cl try 0)
  go :: (Num c, Ord c) => ChanceList Int c -> c -> c -> Int -> Int
  go cl dsr cpf tries
    | dsr <= 1 - cpf
    = tries
    | otherwise
    = let cpf' = calculateCPF cl (tries + 1) cpf
      in  go (reduceCL cl tries) dsr cpf' (tries + 1)

-- | Given a ChanceList and the number of tries tried, returns the chance of the event having happened atleast once. 
chanceForCl :: Num c => ChanceList Int c -> Int -> c
chanceForCl cl tries = go cl tries 1
 where
  go :: Num c => ChanceList Int c -> Int -> c -> c
  go cl try pf
    | try == 0
    = 1 - pf
    | otherwise
    = let pf' = pf * (1 - getChance cl try 0) in go cl (try - 1) pf'



