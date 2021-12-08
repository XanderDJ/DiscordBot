{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Pokemon.DamageCalc.Functions where

import qualified Codec.Xlsx.Types.DataValidation as L
import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import qualified Data.List as L
import Data.List.Utility (hasAny)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Pokemon.DamageCalc.Types
import Pokemon.Functions
import Pokemon.TypeMatchups
import Pokemon.Types
import PokemonDB.Types (ItemT (isBerry), MoveT (moveType))
import Text.Read (readMaybe)

getMoveOrder :: EffectivePokemon -> EffectivePokemon -> EffectiveMove -> Environment -> MoveOrder
getMoveOrder attacker defender move env
  | prio > 0 = FIRST
  | prio < 0 = LAST
  | otherwise = if trickRoom env then flipOrder order else order
  where
    attackerSpeed = getSpeedAttacker attacker env
    defenderSpeed = getSpeedDefender defender
    order = if attackerSpeed >= defenderSpeed then FIRST else LAST
    flipOrder FIRST = LAST
    flipOrder LAST = FIRST
    prio = emPriority move

getSpeedAttacker attacker env = if tailWind env then (fromIntegral . getSpeed) attacker *// 2 else getSpeed attacker

getSpeedDefender = getSpeed

getSpeed :: t -> a0
getSpeed = error "not implemented"

getBp :: EffectiveMove -> EffectivePokemon -> EffectivePokemon -> Int
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

getDefensiveAbilityMultiplier :: T.Text -> EffectivePokemon -> EffectiveMove -> [Type] -> AttackType -> AttackRelation -> Double
getDefensiveAbilityMultiplier "bulletproof" _ EM {emBullet = isBullet} _ _ _ = if isBullet then 0 else 1
getDefensiveAbilityMultiplier "filter" _ _ _ _ ar = if ar `elem` [SuperEffective, StronglyEffective] then 0.75 else 1
getDefensiveAbilityMultiplier "solidrock" _ _ _ _ ar = if ar `elem` [SuperEffective, StronglyEffective] then 0.75 else 1
getDefensiveAbilityMultiplier "fluffy" _ EM {isContact = contact} typing _ _
  | contact && FIRE `elem` typing = 1
  | contact = 0.5
  | FIRE `elem` typing = 2
  | otherwise = 1
getDefensiveAbilityMultiplier "icescales" _ _ _ SPECIAL _ = 0.5
getDefensiveAbilityMultiplier "heatproof" _ _ typing _ _ = if FIRE `elem` typing then 0.5 else 1
getDefensiveAbilityMultiplier "multiscale" EP {epHPPercentage = percentage} _ _ _ _ = if percentage == 100 then 0.5 else 1
getDefensiveAbilityMultiplier "shadowshield" EP {epHPPercentage = percentage} _ _ _ _ = if percentage == 100 then 0.5 else 1
getDefensiveAbilityMultiplier "prismarmor" _ _ _ _ ar = if ar `elem` [SuperEffective, StronglyEffective] then 0.75 else 1
getDefensiveAbilityMultiplier "punkrock" _ EM {emSound = isSound} _ _ _ = if isSound then 0.5 else 1
getDefensiveAbilityMultiplier "soundproof" _ EM {emSound = isSound} _ _ _ = if isSound then 0 else 1
getDefensiveAbilityMultiplier "waterbubble" _ _ typing _ _ = if FIRE `elem` typing then 0.5 else 1
getDefensiveAbilityMultiplier "aurabreak" _ _ typing _ _ = if hasAny [FAIRY, DARK] typing then 3/4 else 1
getDefensiveAbilityMultiplier "damp" _ EM { emName = name} _ _ _ = if toId name `elem` ["selfdestruct", "explosion", "mindblown", "mistyexplosion"] then 0 else 1
getDefensiveAbilityMultiplier "dazzling" _ EM {emPriority = prio} _ _ _ = if prio > 0 then 0 else 1
getDefensiveAbilityMultiplier "dryskin" _ _ typing _ _ = if FIRE `elem` typing then 1.25 else 1
getDefensiveAbilityMultiplier "queenlymajesty" _ EM {emPriority = prio} _ _ _ = if prio > 0 then 0 else 1
getDefensiveAbilityMultiplier ab ep em typing cat super = 1

getPokemonType :: EffectivePokemon -> [Type]
getPokemonType ep@EP {epName = "silvally"} = fromMaybe (epTyping ep) (epItem ep >>= iOnMemory >>= (fmap (: []) . readMaybe . T.unpack))
getPokemonType p@EP {epName = "arceus"} = fromMaybe (epTyping p) (epItem p >>= iOnPlate >>= (fmap (: []) . readMaybe . T.unpack))
getPokemonType ep = epTyping ep

getMoveBaseType :: EffectivePokemon -> EffectivePokemon -> T.Text -> Environment -> Type -> [Type]
getMoveBaseType _ _ "flyingpress" _ t = [t, FLYING]
getMoveBaseType p _ "multiattack" Env {magicRoom = isRoom} t =
  if not $ "silvally" `T.isInfixOf` (toId . epName) p || (toId . epAbility) p == "klutz" || isRoom
    then [t]
    else fromMaybe [t] (epItem p >>= iOnMemory >>= (fmap (: []) . readMaybe . T.unpack))
getMoveBaseType p _ "judgement" Env {magicRoom = isRoom} t =
  if not $ "arceus" `T.isInfixOf` (toId . epName) p || (toId . epAbility) p == "klutz" || isRoom
    then [t]
    else fromMaybe [t] (epItem p >>= iOnPlate >>= (fmap (: []) . readMaybe . T.unpack))
getMoveBaseType p _ "technoblast" Env {magicRoom = isRoom} t =
  if not $ "genesect" `T.isInfixOf` (toId . epName) p || (toId . epAbility) p == "klutz" || isRoom
    then [t]
    else fromMaybe [t] (epItem p >>= iOnDrive >>= (fmap (: []) . readMaybe . T.unpack))
getMoveBaseType EP {epTyping = typing} _ "revelationdance" _ _ = [head typing]
getMoveBaseType EP {epAbility = "normalize"} _ _ _ _ = [NORMAL]
getMoveBaseType EP {epItem = item, epAbility = ability} EP {epAbility = a'} "naturalgift" Env {magicRoom = isRoom} t =
  fromMaybe
    [NORMAL]
    ( item
        >>= \i ->
          if (not . iBerry) i || toId ability == "klutz" || isRoom || toId a' == "unnerve"
            then Just [NORMAL]
            else case toId . iName $ i of
              "cheriberry" -> Just [FIRE]
              "chestoberry" -> Just [WATER]
              "pechaberry" -> Just [ELECTRIC]
              "rawstberry" -> Just [GRASS]
              "aspearberry" -> Just [ICE]
              "leppaberry" -> Just [FIGHTING]
              "oranberry" -> Just [POISON]
              "persimberry" -> Just [GROUND]
              "lumberry" -> Just [FLYING]
              "sitrusberry" -> Just [PSYCHIC]
              "figyberry" -> Just [BUG]
              "wikiberry" -> Just [ROCK]
              "magoberry" -> Just [GHOST]
              "aguavberry" -> Just [DRAGON]
              "iapapaberry" -> Just [DARK]
              "razzberry" -> Just [STEEL]
              "blukberry" -> Just [FIRE]
              "nanabberry" -> Just [WATER]
              "wepearberry" -> Just [ELECTRIC]
              "pinapberry" -> Just [GRASS]
              "pomegberry" -> Just [ICE]
              "kelpsyberry" -> Just [FIGHTING]
              "qualotberry" -> Just [POISON]
              "hondewberry" -> Just [GROUND]
              "grepaberry" -> Just [FLYING]
              "tamatoberry" -> Just [PSYCHIC]
              "cornnberry" -> Just [BUG]
              "magostberry" -> Just [ROCK]
              "rabutaberry" -> Just [GHOST]
              "nomelberry" -> Just [DRAGON]
              "spelonberry" -> Just [DARK]
              "pamtreberry" -> Just [STEEL]
              "watmelberry" -> Just [FIRE]
              "durinberry" -> Just [WATER]
              "belueberry" -> Just [ELECTRIC]
              "occaberry" -> Just [FIRE]
              "passhoberry" -> Just [WATER]
              "wacanberry" -> Just [ELECTRIC]
              "rindoberry" -> Just [GRASS]
              "yacheberry" -> Just [ICE]
              "chopleberry" -> Just [FIGHTING]
              "kebiaberry" -> Just [POISON]
              "shucaberry" -> Just [GROUND]
              "cobaberry" -> Just [FLYING]
              "payapaberry" -> Just [PSYCHIC]
              "tangaberry" -> Just [BUG]
              "chartiberry" -> Just [ROCK]
              "kasibberry" -> Just [GHOST]
              "habanberry" -> Just [DRAGON]
              "colburberry" -> Just [DARK]
              "babiriberry" -> Just [STEEL]
              "chilanberry" -> Just [NORMAL]
              "liechieberry" -> Just [GRASS]
              "ganlonberry" -> Just [ICE]
              "salacberry" -> Just [FIGHTING]
              "petayaberry" -> Just [POISON]
              "apicotberry" -> Just [GROUND]
              "lansatberry" -> Just [FLYING]
              "starfberry" -> Just [PSYCHIC]
              "enigmaberry" -> Just [BUG]
              "micleberry" -> Just [ROCK]
              "custapberry" -> Just [GHOST]
              "jabocaberry" -> Just [DRAGON]
              "rowapberry" -> Just [DARK]
              "roseliberry" -> Just [FAIRY]
              "keeberry" -> Just [FAIRY]
              "marangaberry" -> Just [DARK]
              _ -> Nothing
    )
getMoveBaseType EP {epAbility = ability} _ "naturepower" Env {activeTerrain = terrain} t =
  fromMaybe
    [t]
    ( terrain >>= \case
        ELECTRIC_T -> Just [ELECTRIC]
        PSYCHIC_T -> Just [PSYCHIC]
        MISTY -> Just [FAIRY]
        GRASSY -> Just [GRASS]
    )
getMoveBaseType _ _ _ _ t = [t]

ateAbility :: T.Text -> [Type] -> [Type]
ateAbility "aerilate" [NORMAL] = [FLYING]
ateAbility "pixilate" [NORMAL] = [FAIRY]
ateAbility "refrigerate" [NORMAL] = [ICE]
ateAbility "galvanize" [NORMAL] = [ELECTRIC]
ateAbility _ t = t

liquidVoice :: EffectivePokemon -> EffectiveMove -> [Type] -> [Type]
liquidVoice EP {epAbility = ability} EM {emSound = sound} ts = if toId ability == "liquidvoice" && sound then [WATER] else ts

electrify :: Environment -> [Type] -> [Type]
electrify Env {electrified = electrified} ts = if electrified && head ts == NORMAL then [ELECTRIC] else ts

getMoveType :: EffectivePokemon -> EffectivePokemon -> Environment -> EffectiveMove -> [Type]
getMoveType attacker defender env move = (getMoveBaseType attacker defender moveName env >>> ateAbility attackerAbility >>> liquidVoice attacker move >>> electrify env) (emType move)
  where
    moveName = emName move
    attackerAbility = epAbility attacker

updateTmWithItem :: T.Text -> Bool -> TypeMatchup -> TypeMatchup
updateTmWithItem "airballoon" b tm@(TM am dm) = if not b then let dm' = M.insert FLYING Immune dm in TM am dm' else tm
updateTmWithItem "ironball" b tm@(TM am dm) = if not b then let dm' = M.insert GROUND Neutral dm in TM am dm' else tm
updateTmWithItem "ringtarget" b tm = if not b then removeImmunities tm else tm
updateTmWithItem _ _ tm = tm

enrichTmWithAbility :: T.Text -> T.Text -> T.Text -> TypeMatchup -> TypeMatchup
enrichTmWithAbility atkAbility defAbility moveName tm@(TM am dm)
  | atkAbility `elem` abilityIgnoringAbilities || atkAbility == "neutralizinggas" = tm
  | moveName `elem` movesThatIgnoreAbilities = tm
  | defAbility `elem` ["waterabsorb", "dryskin", "stormdrain"] = TM am (M.insert WATER Immune dm)
  | defAbility == "flashfire" = TM am (M.insert FIRE Immune dm)
  | defAbility == "levitate" = TM am (M.insert GROUND Immune dm)
  | defAbility `elem` ["motordrive", "voltabsorb", "lightningrod"] = TM am (M.insert ELECTRIC Immune dm)
  | defAbility == "sapsipper" = TM am (M.insert GRASS Immune dm)
  | defAbility == "wonderguard" = TM am wonderguardDm
  | otherwise = tm
  where
    wonderguardFilter StronglyEffective = Just StronglyEffective
    wonderguardFilter SuperEffective = Just SuperEffective
    wonderguardFilter x = Just Immune
    wonderguardDm' = foldl (flip $ M.update wonderguardFilter) dm (M.keys dm)
    allTypesNotPresent = allTypes L.\\ M.keys dm
    wonderguardDm = foldl (\m a -> M.insert a Immune m) wonderguardDm' allTypesNotPresent

thousandArrows :: T.Text -> Typing -> Int -> TypeMatchup -> TypeMatchup
thousandArrows "thousandarrows" defenderTyping tu (TM am dm) = let relation = if tu > 0 || FLYING `notElem` defenderTyping then Neutral else Resisted in TM am (M.insert GROUND relation dm)
thousandArrows _ _ _ tm = tm

getItemMultiplier :: T.Text -> EffectiveMove -> Typing -> AttackRelation -> T.Text -> (T.Text, T.Text) -> Double
getItemMultiplier "latios" _ moveType _ "souldew" _ = if hasAny moveType [DRAGON, PSYCHIC] then 1.2 else 1
getItemMultiplier "latias" _ moveType _ "souldew" _ = if hasAny moveType [DRAGON, PSYCHIC] then 1.2 else 1
getItemMultiplier "dialga" _ moveType _ "adamantorb" _ = if hasAny moveType [DRAGON, STEEL] then 1.2 else 1
getItemMultiplier "palkia" _ moveType _ "lustrousorb" _ = if hasAny moveType [DRAGON, WATER] then 1.2 else 1
getItemMultiplier "giratina" _ moveType _ "griseuosorb" _ = if hasAny moveType [DRAGON, GHOST] then 1.2 else 1
getItemMultiplier "giratinaorigin" _ moveType _ _ _ = if hasAny moveType [DRAGON, GHOST] then 1.2 else 1
getItemMultiplier _ em _ _ "metronome" _ = min 2 (1 + 0.2 * fromIntegral (emTimesUsed em))
getItemMultiplier _ _ _ _ "lifeorb" _ = 1.3
getItemMultiplier _ _ _ SuperEffective "expertbelt" _ = 1.2
getItemMultiplier _ _ moveType _ "normalgem" _ = getGemMultiplier NORMAL moveType
getItemMultiplier _ _ moveType _ "pinkbow" _ = getBowMultiplier NORMAL moveType
getItemMultiplier _ _ moveType _ "polkadotbow" _ = getBowMultiplier NORMAL moveType
getItemMultiplier _ _ moveType _ "silkscarf" _ = getItemTypeMultiplier NORMAL moveType
getItemMultiplier _ _ moveType _ "darkgem" _ = getGemMultiplier DARK moveType
getItemMultiplier _ _ moveType _ "dreadplate" _ = getPlateMultiplier DARK moveType
getItemMultiplier _ _ moveType _ "blackglasses" _ = getItemTypeMultiplier DARK moveType
getItemMultiplier _ _ moveType _ "psychicgem" _ = getGemMultiplier PSYCHIC moveType
getItemMultiplier _ _ moveType _ "twistedspoon" _ = getItemTypeMultiplier PSYCHIC moveType
getItemMultiplier _ _ moveType _ "oddincense" _ = getItemTypeMultiplier PSYCHIC moveType
getItemMultiplier _ _ moveType _ "mindplate" _ = getPlateMultiplier PSYCHIC moveType
getItemMultiplier _ _ moveType _ "ghostgem" _ = getGemMultiplier GHOST moveType
getItemMultiplier _ _ moveType _ "spelltag" _ = getItemTypeMultiplier GHOST moveType
getItemMultiplier _ _ moveType _ "spookyplate" _ = getPlateMultiplier GHOST moveType
getItemMultiplier _ _ moveType _ "flyinggem" _ = getGemMultiplier FLYING moveType
getItemMultiplier _ _ moveType _ "sharpbeak" _ = getItemTypeMultiplier FLYING moveType
getItemMultiplier _ _ moveType _ "skyplate" _ = getPlateMultiplier FLYING moveType
getItemMultiplier _ _ moveType _ "fightinggem" _ = getGemMultiplier FIGHTING moveType
getItemMultiplier _ _ moveType _ "blackbelt" _ = getItemTypeMultiplier FIGHTING moveType
getItemMultiplier _ _ moveType _ "fistplate" _ = getPlateMultiplier FIGHTING moveType
getItemMultiplier _ _ moveType _ "icegem" _ = getGemMultiplier ICE moveType
getItemMultiplier _ _ moveType _ "nevermeltice" _ = getItemTypeMultiplier ICE moveType
getItemMultiplier _ _ moveType _ "icicleplate" _ = getPlateMultiplier ICE moveType
getItemMultiplier _ _ moveType _ "steelgem" _ = getGemMultiplier STEEL moveType
getItemMultiplier _ _ moveType _ "metalcoat" _ = getItemTypeMultiplier STEEL moveType
getItemMultiplier _ _ moveType _ "ironplate" _ = getPlateMultiplier STEEL moveType
getItemMultiplier _ _ moveType _ "rockgem" _ = getGemMultiplier ROCK moveType
getItemMultiplier _ _ moveType _ "rockincense" _ = getItemTypeMultiplier ROCK moveType
getItemMultiplier _ _ moveType _ "hardstone" _ = getItemTypeMultiplier ROCK moveType
getItemMultiplier _ _ moveType _ "stoneplate" _ = getPlateMultiplier ROCK moveType
getItemMultiplier _ _ moveType _ "poisongem" _ = getGemMultiplier POISON moveType
getItemMultiplier _ _ moveType _ "poisonbarb" _ = getGemMultiplier POISON moveType
getItemMultiplier _ _ moveType _ "toxicplate" _ = getPlateMultiplier POISON moveType
getItemMultiplier _ _ moveType _ "groundgem" _ = getGemMultiplier GROUND moveType
getItemMultiplier _ _ moveType _ "softsand" _ = getItemTypeMultiplier GROUND moveType
getItemMultiplier _ _ moveType _ "earthplate" _ = getPlateMultiplier GROUND moveType
getItemMultiplier _ _ moveType _ "firegem" _ = getGemMultiplier FIRE moveType
getItemMultiplier _ _ moveType _ "charcoal" _ = getItemTypeMultiplier FIRE moveType
getItemMultiplier _ _ moveType _ "flameplate" _ = getItemTypeMultiplier FIRE moveType
getItemMultiplier _ _ moveType _ "watergem" _ = getGemMultiplier WATER moveType
getItemMultiplier _ _ moveType _ "mysticwater" _ = getItemTypeMultiplier WATER moveType
getItemMultiplier _ _ moveType _ "seaincense" _ = getItemTypeMultiplier WATER moveType
getItemMultiplier _ _ moveType _ "waveincense" _ = getItemTypeMultiplier WATER moveType
getItemMultiplier _ _ moveType _ "splashplate" _ = getPlateMultiplier WATER moveType
getItemMultiplier _ _ moveType _ "grassgem" _ = getGemMultiplier GRASS moveType
getItemMultiplier _ _ moveType _ "miracleseed" _ = getItemTypeMultiplier GRASS moveType
getItemMultiplier _ _ moveType _ "roseincense" _ = getItemTypeMultiplier GRASS moveType
getItemMultiplier _ _ moveType _ "meadowplate" _ = getPlateMultiplier GRASS moveType
getItemMultiplier _ _ moveType _ "electricgem" _ = getGemMultiplier ELECTRIC moveType
getItemMultiplier _ _ moveType _ "magnet" _ = getItemTypeMultiplier ELECTRIC moveType
getItemMultiplier _ _ moveType _ "zapplate" _ = getPlateMultiplier ELECTRIC moveType
getItemMultiplier _ _ moveType _ "fairygem" _ = getGemMultiplier FAIRY moveType
getItemMultiplier _ _ moveType _ "pixieplate" _ = getPlateMultiplier FAIRY moveType
getItemMultiplier _ _ moveType _ "buggem" _ = getGemMultiplier BUG moveType
getItemMultiplier _ _ moveType _ "silverpowder" _ = getItemTypeMultiplier BUG moveType
getItemMultiplier _ _ moveType _ "insectplate" _ = getPlateMultiplier BUG moveType
getItemMultiplier _ _ moveType _ "dragongem" _ = getGemMultiplier DRAGON moveType
getItemMultiplier _ _ moveType _ "dragonfang" _ = getItemTypeMultiplier DRAGON moveType
getItemMultiplier _ _ moveType _ "dracoplate" _ = getPlateMultiplier DRAGON moveType
getItemMultiplier _ _ moveType _ "chilanberry" _ = getBerryMultiplier NORMAL moveType
getItemMultiplier _ _ moveType SuperEffective "babiriberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier STEEL moveType else 1
getItemMultiplier _ _ moveType SuperEffective "chartiberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ROCK moveType else 1
getItemMultiplier _ _ moveType SuperEffective "chopleberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FIGHTING moveType else 1
getItemMultiplier _ _ moveType SuperEffective "cobaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FLYING moveType else 1
getItemMultiplier _ _ moveType SuperEffective "colburberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier DARK moveType else 1
getItemMultiplier _ _ moveType SuperEffective "habanberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier DRAGON moveType else 1
getItemMultiplier _ _ moveType SuperEffective "kasibberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GHOST moveType else 1
getItemMultiplier _ _ moveType SuperEffective "kebiaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier POISON moveType else 1
getItemMultiplier _ _ moveType SuperEffective "occaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier FIRE moveType else 1
getItemMultiplier _ _ moveType SuperEffective "passhoberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier WATER moveType else 1
getItemMultiplier _ _ moveType SuperEffective "payapaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier PSYCHIC moveType else 1
getItemMultiplier _ _ moveType SuperEffective "rindoberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GRASS moveType else 1
getItemMultiplier _ _ moveType SuperEffective "shucaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier GROUND moveType else 1
getItemMultiplier _ _ moveType SuperEffective "tangaberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier BUG moveType else 1
getItemMultiplier _ _ moveType SuperEffective "wacanberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ELECTRIC moveType else 1
getItemMultiplier _ _ moveType SuperEffective "yacheberry" a = if fst a /= "unnerve" || snd a == "neutralizinggas" then getBerryMultiplier ICE moveType else 1
getItemMultiplier _ _ _ _ _ _ = 1

getGeneralTypeMultiplier :: (Foldable t, Eq a, Num p) => p -> a -> t a -> p
getGeneralTypeMultiplier mult tipe moveType = if tipe `elem` moveType then mult else 1

getItemTypeMultiplier :: Type -> [Type] -> Double
getItemTypeMultiplier = getGeneralTypeMultiplier 1.2

getGemMultiplier :: Type -> [Type] -> Double
getGemMultiplier = getGeneralTypeMultiplier 1.3

getBowMultiplier :: Type -> [Type] -> Double
getBowMultiplier = getGeneralTypeMultiplier 1.2

getPlateMultiplier :: Type -> [Type] -> Double
getPlateMultiplier = getGeneralTypeMultiplier 1.2

getBerryMultiplier :: Type -> [Type] -> Double
getBerryMultiplier = getGeneralTypeMultiplier 0.5

getScreenMultiplier :: [Screen] -> T.Text -> Bool -> AttackType -> Double
getScreenMultiplier [] _ _ _ = 1
getScreenMultiplier (REFLECT : ss) a b t = if t == PHYSICAL && a /= "infiltrator" && not b then 0.5 else getScreenMultiplier ss a b t
getScreenMultiplier (LIGHT_SCREEN : ss) a b t = if t == SPECIAL && a /= "infiltrator" && not b then 0.5 else getScreenMultiplier ss a b t
getScreenMultiplier (AURORA_VEIL : ss) a b t = if a /= "infiltrator" && not b then 0.5 else 1

getWeatherMult :: [Type] -> Weather -> Double
getWeatherMult mType RAIN
  | hasAny mType [WATER] = 1.5
  | hasAny mType [FIRE] = 0.5
  | otherwise = 1
getWeatherMult mType SUN
  | hasAny mType [FIRE] = 1.5
  | hasAny mType [WATER] = 0.5
  | otherwise = 1
getWeatherMult _ _ = 1

isGrounded :: EffectivePokemon -> Bool
isGrounded EP {..} = grounded
  where
    ts = epTyping
    ability = toId epAbility
    item = fromMaybe "" (epItem <&> toId . iName)
    grounded = FLYING `elem` ts || ability == "levitate" || item == "airballoon" || epRisen