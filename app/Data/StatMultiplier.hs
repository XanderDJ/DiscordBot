module Data.StatMultiplier (StatMultiplier, (%%), raise, raiseSharply, raiseDrastically, lower, lowerSharply, lowerDrastically, getMultiplier) where

type PositiveBoosts = Int

type NegativeBoosts = Int

data StatMultiplier = SM PositiveBoosts NegativeBoosts deriving (Show, Eq)

(%%) :: Int -> Int -> StatMultiplier
(%%) pb nb = if pb > nb then SM (pb - nb) 0 else SM 0 (nb - pb)

infixl 4 %%

instance Ord StatMultiplier where
  (SM pb nb) <= (SM pb' nb') = pb - nb <= pb' - nb'

instance Num StatMultiplier where
  (SM pb nb) + (SM pb' nb') = pb + pb' %% nb + nb'
  (*) (SM pb nb) (SM pb' nb') = pb * pb' %% nb * nb'
  abs = id
  signum (SM pb nb) = signum pb %% signum nb
  fromInteger i = if i < 0 then 0 %% abs (fromInteger i) else fromInteger i %% 0
  negate (SM pb nb) = SM nb pb

raise :: StatMultiplier -> StatMultiplier
raise = (+) (1 %% 0)

raiseSharply :: StatMultiplier -> StatMultiplier
raiseSharply = raise . raise

raiseDrastically :: StatMultiplier -> StatMultiplier
raiseDrastically = raise . raise . raise

lower :: StatMultiplier -> StatMultiplier
lower = (+) (0 %% 1)

lowerSharply :: StatMultiplier -> StatMultiplier
lowerSharply = lower . lower

lowerDrastically :: StatMultiplier -> StatMultiplier
lowerDrastically = lower . lower . lower

getMultiplier :: StatMultiplier -> Double
getMultiplier (SM pb nb) = (fromIntegral pb + 2) / (fromIntegral nb + 2)
