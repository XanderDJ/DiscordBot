module Pokemon.DamageCalc.Functions where

import Pokemon.DamageCalc.Types
import Pokemon.Types

getBp :: EffectiveMove  -> EffectivePokemon -> EffectivePokemon -> Int
getBp m a d = undefined

getAttackStat :: Maybe Item -> AttackType -> EffectivePokemon -> Int
getAttackStat i = getAttackStat'

getAttackStat' :: AttackType -> EffectivePokemon -> Int
getAttackStat' PHYSICAL EP {epName = p, epLevel = lvl, epEvs = evs, epIvs = ivs} = undefined
getAttackStat' SPECIAL EP {epName = name, epLevel = lvl, epEvs = evs, epIvs = ivs} = undefined
getAttackStat' OTHER p = 0

getDefenseStat :: Maybe Item -> AttackType -> EffectivePokemon -> Int
getDefenseStat i = getDefenseStat'

getDefenseStat' :: AttackType -> EffectivePokemon -> Int
getDefenseStat' PHYSICAL p = undefined
getDefenseStat' SPECIAL p = undefined
getDefenseStat' OTHER p = undefined