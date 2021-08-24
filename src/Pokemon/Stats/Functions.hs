module Pokemon.Stats.Functions where

import Data.Map
import qualified Data.Text as T
import Pokemon.Functions
import Pokemon.Replays.Types
import Pokemon.Stats.Types hiding (hazards)
import Pokemon.Types hiding (Move)

getOppositePositions :: Position -> (Position, Position)
getOppositePositions P1 = (P2, P4)
getOppositePositions P2 = (P1, P3)
getOppositePositions P3 = (P2, P4)
getOppositePositions P4 = (P1, P3)

-- | if position or name not in the match state then this will throw an error
getPokeStats :: Position -> T.Text -> Match -> PokeStats
getPokeStats pos name match = pokeStats (players match ! pos) ! name

-- | if the name is not in the match then this will throw an error
getName :: Nick -> Match -> T.Text
getName (pos, name) match = nicks match ! name

getStatusCause :: T.Text -> Status -> Match -> T.Text
getStatusCause name status match = pokeStatusCause match ! (name, status)

-- | Check if move is util
isUtil :: MoveType -> Bool
isUtil BOOST = False
isUtil ATTACK = False
isUtil _ = True

isUtilMove :: T.Text -> Bool
isUtilMove moveName =
  let name = moveString moveName
   in elem name utility || elem name hazards || elem name recovery || elem name statusMoves || elem name other

-- | change move name to pokeapi standard (lowercase with dashes)
moveString :: T.Text -> String
moveString txt = T.unpack $ (T.toLower . T.replace " " "-") txt

putMajorAction :: ReplayMessage -> Match -> Match
putMajorAction rm match = match {currentMajorAction = rm}

updateUtility :: ReplayMessage -> Match -> Match
updateUtility (RMMove (Move (pos, nick) mName _ tags)) match =
  let name = getName (pos, nick) match
      ps = getPokeStats pos name match
      bool = not (alreadyUtil name match) && isUtilMove mName
      turnsUtil' = if bool then turnsUtil ps + 1 else turnsUtil ps
      turnsActive' = turnsActive ps + 1
      uf' = fromIntegral turnsUtil' / fromIntegral turnsActive'
      ps' = ps {turnsActive = turnsActive', turnsUtil = turnsUtil', utilityFactor = uf'}    
      match' = if bool then match {utilityMap = insert name True (utilityMap match)} else match
   in putPokeStats pos name ps' match'
updateUtility (RMSwitch (Switch (pos, nick) name health tags)) match =
    let 
        ps = getPokeStats pos (getName (pos, nick) match) match
        turnsUtil' = turnsUtil ps + 1
        turnsActive' = turnsActive ps + 1
        uf' = fromIntegral turnsUtil' / fromIntegral turnsActive'
        ps' = ps {turnsActive = turnsActive', turnsUtil = turnsUtil', utilityFactor = uf'}    
        match' = match {utilityMap = insert name True (utilityMap match)}
    in
        putPokeStats pos name ps' match'
updateUtility _ match = match

putPokeStats :: Position -> T.Text -> PokeStats -> Match -> Match
putPokeStats pos name ps match = match'
  where
    playerStats = players match ! pos
    ps' = insert name ps (pokeStats playerStats)
    playerStats' = playerStats {pokeStats = ps'}
    match' = match {players = insert pos playerStats' (players match)}

alreadyUtil :: T.Text -> Match -> Bool
alreadyUtil name match = fBool
  where
    fBool = findWithDefault False name (utilityMap match)

updateDeathMap :: Position -> Match -> Match
updateDeathMap pos match = match { diedMap = insert pos True (diedMap match)}

updateDeath :: Nick -> Match -> Match
updateDeath (pos, nick) match = putPokeStats pos name ps' match {pokeHealth = insert nick (0, totalHp , "fnt") (pokeHealth match)}
 where
     name = getName (pos, nick) match
     ps = getPokeStats pos name match
     ps' = ps { timesDied = timesDied ps + 1}
     (_,totalHp, _) = pokeHealth match ! nick


