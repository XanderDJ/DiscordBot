module Pokemon.DamageCalc.DamageCalc where

import Control.Monad.Reader
import Pokemon.DamageCalc.Types
import Pokemon.DamageCalc.Functions
import Pokemon.Types
import Pokemon.Functions
import Data.StatMultiplier


runCalc :: DCS -> (Int, Int)
runCalc = runReader calcDamage


calcDamage :: Reader DCS (Int, Int)
calcDamage =
  baseDamage
    >>= targetsMultiplier
    >>= weatherMultiplier
    >>= criticalHitMultiplier
    >>= stabMultiplier
    >>= typeEffectivenessMultiplier
    >>= burnMultiplier
    >>= screensMultiplier
    >>= minimizeMultiplier
    >>= digMultiplier
    >>= diveMultiplier
    >>= abilityMultiplier
    >>= itemMultiplier
    >>= randomMultiplier

baseDamage :: Reader DCS Int
baseDamage = do
    attacker <- getAttackingPokemon
    defender <- getDefendingPokemon
    move <- getMove
    let bp = getBp move attacker defender
        moveType = emCategory  move
        atk =  fromIntegral (getAttackStat (epItem attacker) moveType attacker) *// (getMultiplier . epMultiplier) attacker
        def = fromIntegral (getDefenseStat (epItem defender) moveType defender) *// (getMultiplier . epMultiplier) defender
        lvl = epLevel attacker
    return $ div ((div (2 * lvl) 5 + 2) * bp * div atk def) 50 + 2

targetsMultiplier :: Int -> Reader DCS Int
targetsMultiplier dmg = undefined

weatherMultiplier :: Int -> Reader DCS Int
weatherMultiplier dmg = undefined

terrainMultiplier :: Int -> Reader DCS Int
terrainMultiplier dmg = undefined

criticalHitMultiplier :: Int -> Reader DCS Int
criticalHitMultiplier dmg = undefined

stabMultiplier :: Int -> Reader DCS Int
stabMultiplier dmg = undefined


typeEffectivenessMultiplier :: Int -> Reader DCS Int
typeEffectivenessMultiplier dmg = undefined

burnMultiplier :: Int -> Reader DCS Int
burnMultiplier dmg = undefined

screensMultiplier :: Int -> Reader DCS Int
screensMultiplier dmg = undefined

minimizeMultiplier :: Int -> Reader DCS Int
minimizeMultiplier dmg = undefined

digMultiplier :: Int -> Reader DCS Int
digMultiplier dmg = undefined

diveMultiplier :: Int -> Reader DCS Int
diveMultiplier dmg = undefined

abilityMultiplier :: Int -> Reader DCS Int
abilityMultiplier dmg = undefined

itemMultiplier :: Int -> Reader DCS Int
itemMultiplier dmg = undefined

randomMultiplier :: Int -> Reader DCS (Int, Int)
randomMultiplier dmg = pure (fromIntegral dmg *// 0.85, dmg)


-- HELPER FUNCTIONS

getEnvironment :: Reader DCS Environment
getEnvironment = reader f
 where f (DCS env _ _ _) = env

getAttackingPokemon :: Reader DCS EffectivePokemon 
getAttackingPokemon = reader f
 where f (DCS _ p _ _) = p

getDefendingPokemon :: Reader DCS EffectivePokemon 
getDefendingPokemon = reader f
 where f (DCS _ _ p _) = p

getMove :: Reader DCS EffectiveMove 
getMove = reader f
 where f (DCS _ _ _ move) = move
