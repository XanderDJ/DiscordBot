module Pokemon.Functions where

import Data.List (sortBy)
import Data.Maybe
import Data.Ord (Down (Down))
import qualified Data.Text as T
import Data.Ratio (denominator, numerator, (%))
import Pokemon.Nature
import Pokemon.Types
import Text.Read (readMaybe)

getType :: String -> Maybe Type
getType = readMaybe

getTyping :: String -> Maybe Typing
getTyping = readMaybe

getTypes :: [String] -> Maybe Typing
getTypes = mapM getType

-- | given a string and a value make a basestat
mkStat :: String -> Int -> BaseStat
mkStat name value = let stat = getStat name in BaseStat stat value

-- | Changes string representing stat into Stat type
getStat :: String -> Stat
getStat "hp" = HP
getStat "attack" = ATK
getStat "atk" = ATK
getStat "defense" = DEF
getStat "def" = DEF
getStat "special-attack" = SPATK
getStat "spa" = SPATK
getStat "special-defense" = SPDEF
getStat "spd" = SPDEF
getStat "speed" = SPEED
getStat "spe" = SPEED
getStat s = error $ s ++ " is not a stat."

getNatureEffect :: String -> NatureEffect
getNatureEffect "positive" = NPositive
getNatureEffect "pos" = NPositive
getNatureEffect "neutral" = NNeutral
getNatureEffect "neu" = NNeutral
getNatureEffect "negative" = NNegative
getNatureEffect "neg" = NNegative
getNatureEffect _ = NNeutral

-- | Get the base stat of a pokemon using a string as a name
getBaseStat :: String -> Pokemon -> BaseStat
getBaseStat name pok = findBaseStat basestats stat
  where
    basestats = baseStats pok
    stat = getStat name

findBaseStat :: BaseStats -> Stat -> BaseStat
findBaseStat [] s = error $ "Couldn't find stat \"" ++ show s ++ "\" in base stats"
findBaseStat ((BaseStat s' val) : bs) s = if s' == s then BaseStat s val else findBaseStat bs s

minStatAt :: Level -> BaseStat -> Int
minStatAt lvl = calcStat lvl 0 0 NNegative

noInvestStatAt :: Level -> BaseStat -> Int
noInvestStatAt lvl = calcStat lvl 31 0 NNeutral

maxStatAt :: Level -> BaseStat -> Int
maxStatAt lvl = calcStat lvl 31 252 NPositive

neutralMaxStatAt :: Level -> BaseStat -> Int
neutralMaxStatAt lvl = calcStat lvl 31 252 NNeutral

calcStat :: Level -> Int -> Int -> NatureEffect -> BaseStat -> Int
calcStat lvl iv ev natureEffect baseStat = case baseStat of
  BaseStat HP n -> (2 * n + iv + div ev 4) * div lvl 100 + 10 + lvl
  BaseStat _ n ->
    let multiplier NNegative = 0.9
        multiplier NNeutral = 1
        multiplier NPositive = 1.1
     in (fromIntegral (2 * n + iv + div ev 4) * (fromIntegral lvl / 100) + 5) *// multiplier natureEffect

getValue :: BaseStat -> Int
getValue (BaseStat _ val) = val

-- | floored multiplication
(*//) :: (RealFrac a, Num a, Integral b) => a -> a -> b
a *// b = floor (a * b)

infixl 7 *//

sortOnSpeed :: [Pokemon] -> [Pokemon]
sortOnSpeed = sortBy (sortPokemon "speed")

-- | Ordering is reversed to make it descending instead of ascending. Shown by the use of Down
sortPokemon :: String -> Pokemon -> Pokemon -> Ordering
sortPokemon stat pok1 pok2 = compare (Down stat1) (Down stat2)
  where
    baseStat1 = getBaseStat stat pok1
    stat1 = getValue baseStat1
    baseStat2 = getBaseStat stat pok2
    stat2 = getValue baseStat2

maxSpeed :: Pokemon -> Int
maxSpeed = maxStatAt 100 . getBaseStat "speed"

maxSpeedWithScarf :: Pokemon -> Int
maxSpeedWithScarf = (*// 1.5) . fromIntegral . maxSpeed

getMoveCategory :: Move -> MoveCategory
getMoveCategory (Move name tipe dClass bp' accuracy _)
  | toName name `elem` hazards = HAZARD
  | toName name `elem` utility = UTILITY
  | toName name `elem` recovery = RECOVERY
  | toName name `elem` statusMoves = STATUS
  | toName name `elem` boostMoves = BOOST
  | dClass == PHYSICAL || dClass == SPECIAL = ATTACK
  | otherwise = REST

toName :: T.Text -> T.Text
toName = T.replace " " "-" . T.toLower

hazards :: [T.Text]
hazards = ["spikes", "toxic-spikes", "stealth-rock", "sticky-web"]

utility :: [T.Text]
utility =
  [ "trick-room",
    "rain-dance",
    "sunny-day",
    "hail",
    "sandstorm",
    "reflect",
    "light-screen",
    "aurora-veil",
    "psychic-terrain",
    "misty-terrain",
    "grassy-terrain",
    "electric-terrain",
    "teleport",
    "knock-off",
    "defog",
    "rapid-spin",
    "trick",
    "switcheroo",
    "magic-coat",
    "protect",
    "substitute",
    "aromatherapy",
    "court-change",
    "encore",
    "taunt",
    "haze",
    "heal-bell",
    "memento",
    "parting-shot",
    "baton-pass",
    "torment"
  ]

other :: [T.Text]
other =
  [ "after-you",
    "ally-switch",
    "aromatic-mist",
    "assist",
    "attract",
    "baby-doll-eyes",
    "barrier",
    "baton-pass",
    "bestow",
    "bide",
    "block",
    "camouflage",
    "captivate",
    "celebrate",
    "charge",
    "charm",
    "chatter",
    "confide",
    "conversion",
    "conversion-2",
    "copycat",
    "corrosive-gas",
    "cotton-spore",
    "crafty-shield",
    "decorate",
    "defense-curl",
    "detect",
    "destiny-bond",
    "disable",
    "eerie-impulse",
    "electrify",
    "embargo",
    "endure",
    "entrainment",
    "fairy-lock",
    "fake-tears",
    "brick-break",
    "psychic-fangs",
    "flash",
    "flower-shield",
    "focus-energy",
    "follow-me",
    "foresight",
    "forests-curse",
    "gastro-acid",
    "gear-up",
    "gravity",
    "growl",
    "grudge",
    "guard-split",
    "guard-swap",
    "harden",
    "happy-hour",
    "heal-block",
    "heal-pulse",
    "heart-stamp",
    "helping-hand",
    "hold-hands",
    "imprison",
    "ingrain",
    "instruct",
    "ion-deluge",
    "kinesis",
    "kings-shield",
    "laser-focus",
    "leer",
    "lock-on",
    "lucky-chant",
    "magic-powder",
    "magic-room",
    "magnet-rise",
    "magnetic-flux",
    "mat-block",
    "me-first",
    "mean-look",
    "metal-sound",
    "mind-reader",
    "miracle-eye",
    "mist",
    "nightmare",
    "noble-roar",
    "obstruct",
    "odor-sleuth",
    "perish-song",
    "play-nice",
    "powder",
    "power-split",
    "power-swap",
    "power-trick",
    "protect",
    "psych-up",
    "psycho-shift",
    "purify",
    "quash",
    "quick-guard",
    "rage-powder",
    "recycle",
    "reflect-type",
    "roar",
    "role-play",
    "safeguard",
    "sand-attack",
    "scary-face",
    "screech",
    "simple-beam",
    "sketch",
    "skill-swap",
    "smokescreen",
    "snatch",
    "soak",
    "speed-swap",
    "spider-web",
    "spiky-shield",
    "spite",
    "spotlight",
    "stockpile",
    "string-shot",
    "substitute",
    "sweet-scent",
    "tail-whip",
    "tailwind",
    "tar-shot",
    "tearful-look",
    "teatime",
    "telekinesis",
    "tickle",
    "topsy-turvy",
    "torment",
    "toxic-thread",
    "transform",
    "trick-or-treat",
    "venom-drench",
    "water-sport",
    "whirlwind",
    "wide-guard",
    "worry-seed"
  ]

recovery :: [T.Text]
recovery =
  [ "moonlight",
    "morning-sun",
    "roost",
    "milk-drink",
    "soft-boiled",
    "wish",
    "shore-up",
    "synthesis",
    "rest",
    "leech-life",
    "absorb",
    "mega-drain",
    "giga-drain",
    "draining-kiss",
    "drain-punch",
    "pain-split",
    "leech-seed",
    "jungle-healing",
    "floral-healing",
    "lunar-dance",
    "life-dew",
    "aqua-ring",
    "healing-wish",
    "heal-pulse",
    "pollen-puff",
    "ingrain",
    "present",
    "slack-off",
    "strength-sap",
    "oblivion-wing",
    "swallow",
    "recover",
    "shore-up"
  ]

statusMoves :: [T.Text]
statusMoves =
  [ "toxic",
    "will-o-wisp",
    "thunder-wave",
    "hypnosis",
    "glare",
    "sleep-powder",
    "stun-spore",
    "dark-void",
    "yawn",
    "confuse-ray",
    "swagger",
    "flatter",
    "poison-powder",
    "poison-gas",
    "sing",
    "sweet-kiss",
    "teeter-dance",
    "encore"
  ]

boostMoves :: [T.Text]
boostMoves =
  [ "acid-armor",
    "acupressure",
    "agility",
    "amnesia",
    "autotomize",
    "belly-drum",
    "bulk-up",
    "calm-mind",
    "charge",
    "clangorous-soul",
    "coil",
    "coaching",
    "cosmic-power",
    "cotton-guard",
    "defend-order",
    "defense-curl",
    "double-team",
    "dragon-dance",
    "focus-energy",
    "geomancy",
    "growth",
    "harden",
    "hone-claws",
    "howl",
    "iron-defense",
    "nasty-plot",
    "no-retreat",
    "quiver-dance",
    "rock-polish",
    "sharpen",
    "rototiller",
    "shell-smash",
    "shift-gear",
    "stuff-cheeks",
    "swords-dance",
    "withdraw",
    "work-up"
  ]

getBp :: Move -> PokemonS -> PokemonS -> Int
getBp m a d = undefined


getAttackStat :: Item -> AttackType -> PokemonS -> Int
getAttackStat i = getAttackStat'

getAttackStat' :: AttackType -> PokemonS -> Int
getAttackStat' PHYSICAL PokemonS {pokemon = p, pLevel = lvl, pEvs = evs, pIvs = ivs} = undefined
getAttackStat' SPECIAL PokemonS {pokemon = p, pLevel = lvl, pEvs = evs, pIvs = ivs} = undefined
getAttackStat' OTHER p = 0

getDefenseStat :: Item -> AttackType -> PokemonS -> Int
getDefenseStat i = getDefenseStat'

getDefenseStat' :: AttackType -> PokemonS -> Int
getDefenseStat' PHYSICAL p = undefined
getDefenseStat' SPECIAL p = undefined
getDefenseStat' OTHER p = undefined