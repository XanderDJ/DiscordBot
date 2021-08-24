module Pokemon.Stats.Stats where

import Control.Monad.State
import Data.Default
import qualified Data.Map as M
import qualified Data.Text as T
import Pokemon.Replays.Types
import Pokemon.Stats.Functions
import Pokemon.Stats.Types

type ReplayMatch = [ReplayMessage]

getStats :: ReplayMatch -> State Match [PlayerStats]
getStats [] = returnPlayers
getStats (RMW win : rms) = returnPlayers
getStats (RMTie tie : rms) = returnPlayers
getStats (RMD delimiter : rms) = handleDelimiter (RMD delimiter) >> getStats rms
getStats (RMPoke poke : rms) = handlePoke poke >> getStats rms
getStats (RMPL player : rms) = handlePlayer player >> getStats rms
getStats (RMTurn turn : rms) = handleTurn turn >> getStats rms
getStats (RMSwitch switch : rms) = handleSwitch switch >> getStats rms
getStats (RMDrag (Drag nick name health tags) : rms) = handleSwitch (Switch nick name health tags) >> getStats rms
getStats (RMMove move : rms) = handleMove move >> getStats rms
getStats (RMF faint : rms) = handleFaint faint >> getStats rms
getStats (RMWeather weather : rms) = handleWeather weather >> getStats rms
getStats (rm : rms) = getStats rms

-- | Handle switch action, put it as major action, initialize pokemon if not already initialized. Put nick into map. Store current hp
handleSwitch :: Switch -> State Match [PlayerStats]
handleSwitch (Switch (pos, nick) name (current, total, status) tags) = do
  match <- get
  -- First initialize mon in map if not already present
  let ps = players match M.! pos
      ps' = ps {pokeStats = M.insertWith keepOld name def (pokeStats ps)}
      -- put nick into map
      ns = nicks match
      ns' = M.insertWith keepOld nick name ns
      -- store current hp
      hps = pokeHealth match
      hps' = M.insert nick (current, total, status) hps
      match' =
        match
          { players = M.insert pos ps' (players match),
            nicks = ns',
            pokeHealth = hps',
            currentMajorAction = RMSwitch (Switch (pos, nick) name (current, total, status) tags)
          }
  put match'
  return []

-- | Handle move command, put it as major action, then check if move was util move and if so check if mon already performed a utility this turn
handleMove :: Move -> State Match [ReplayMatch]
handleMove (Move name moveName mTarget tags) = do
  let (pos, name') = name
  modify (updateAccuracy pos name' tags)
  modify (updateUtility (RMMove (Move name moveName mTarget tags)))
  modify (putMajorAction (RMMove (Move name moveName mTarget tags)))
  return []

updateAccuracy :: Position -> T.Text -> Tags -> Match -> Match
updateAccuracy pos nick tags match = match'
  where
    bool = MISS `elem` tags
    name = nicks match M.! nick
    pls = players match M.! pos
    ps = pokeStats pls M.! name
    ps' =
      let newMovesMade = movesMade ps + 1
          newMovesMissed = if bool then movesMissed ps + 1 else movesMissed ps
       in ps {movesMade = newMovesMade, movesMissed = newMovesMissed, accuracy = fromIntegral (newMovesMade - newMovesMissed) / fromIntegral newMovesMade}
    pls' = pls {pokeStats = M.insert name ps' (pokeStats pls)}
    match' = match {players = M.insert pos pls' (players match)}

-- | Put major action to delimiter, making sure that when minor actions happen at the end of the turn this can be tracked
handleDelimiter :: ReplayMessage -> State Match [PlayerStats]
handleDelimiter delimiter = do
  match <- get
  let match' = match {currentMajorAction = delimiter}
  put match'
  returnPlayers

-- | Correctly initialize players in players map with their positions given
handlePlayer :: Player -> State Match [PlayerStats]
handlePlayer (Pl pos (Just name) _ _) = do
  match <- get
  let match' = match {players = M.insertWith (\a b -> b) pos def {pName = name} (players match)}
  put match'
  returnPlayers
handlePlayer (Pl pos _ _ _) = returnPlayers

-- | Update turns for all players
handleTurn :: Turn -> State Match [PlayerStats]
handleTurn (Turn turn) = do
  match <- get
  let updateTurns turns ps = ps {totalTurns = turns}
      match' = match {players = M.map (updateTurns (fromIntegral turn)) (players match), utilityMap = M.empty, diedMap = M.empty}
  put match'
  returnPlayers

-- | Initialize pokemon for the correct player, for randbats the mons need to be initialized from the switch command
handlePoke :: Poke -> State Match [PlayerStats]
handlePoke (Poke pos name mItem) = do
  match <- get
  let ps = players match M.! pos
      ps' = ps {pokeStats = M.insertWith keepOld name def (pokeStats ps)}
      match' = match {players = M.insert pos ps' (players match)}
  put match'
  returnPlayers

-- | Update died map and put the pokemons health at (0, ""), also update the died attribute in pokestats
handleFaint :: Faint -> State Match [PlayerStats]
handleFaint (Faint (pos, nick)) = do
  modify (updateDeathMap pos)
  modify (updateDeath (pos, nick))
  return []


handleWeather :: WeatherRM -> State Match [PlayerStats]
handleWeather (Weather name tags) = do
  undefined

returnPlayers :: State Match [PlayerStats]
returnPlayers = gets (M.elems . players)

-- HELPER FUNCTIONS

keepOld :: a -> b -> b
keepOld _ b = b