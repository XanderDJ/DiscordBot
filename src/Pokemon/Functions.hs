module Pokemon.Functions where

import Control.Lens (_18')
import Data.List (sort, sortBy)
import Data.Maybe
import Data.Ord (Down (Down))
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Text as T
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
getStat "special-attack" = SPA
getStat "spa" = SPA
getStat "special-defense" = SPD
getStat "spd" = SPD
getStat "speed" = SPE
getStat "spe" = SPE
getStat s = error $ s ++ " is not a stat."

parseNatureEffect :: String -> NatureEffect
parseNatureEffect "positive" = NPositive
parseNatureEffect "pos" = NPositive
parseNatureEffect "neutral" = NNeutral
parseNatureEffect "neu" = NNeutral
parseNatureEffect "negative" = NNegative
parseNatureEffect "neg" = NNegative
parseNatureEffect _ = NNeutral

getNatureEffect :: Stat -> Nature -> NatureEffect
getNatureEffect s (Nature _ pos neg)
  | s == pos = NPositive
  | s == neg = NNegative
  | otherwise = NNeutral

toShowdownRep :: NatureEffect -> String
toShowdownRep NPositive = "+"
toShowdownRep NNegative = "-"
toShowdownRep NNeutral = ""

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
getMoveCategory (Move name tipe dClass bp' _ accuracy _ _ _)
  | toName name `elem` hazards = HAZARD
  | toName name `elem` utility = UTILITY
  | toName name `elem` recovery = RECOVERY
  | toName name `elem` statusMoves = STATUS
  | toName name `elem` boostMoves = BOOST
  | dClass == PHYSICAL || dClass == SPECIAL = ATTACK
  | otherwise = REST

typeToColor :: T.Text -> T.Text
typeToColor "FIRE" = "FFDD6610"
typeToColor "GRASS" = "FF5CA935"
typeToColor "WATER" = "FF386CEB"
typeToColor "ELECTRIC" = "FFF0C108"
typeToColor "FLYING" = "FF9180C4"
typeToColor "GROUND" = "FFD4A82F"
typeToColor "STEEL" = "FF9797BA"
typeToColor "FAIRY" = "FFF540FF"
typeToColor "DARK" = "FF513F34"
typeToColor "GHOST" = "FF554374"
typeToColor "FIGHTING" = "FF9D2721"
typeToColor "NORMAL" = "FF8A8A59"
typeToColor "ROCK" = "FF93802D"
typeToColor "POISON" = "FF803380"
typeToColor "BUG" = "FF8D9A1B"
typeToColor "ICE" = "FF69C6C6"
typeToColor "DRAGON" = "FF4C08EF"
typeToColor "PSYCHIC" = "FFf61C5D"
typeToColor _ = "FF000000"

typeToEmote :: T.Text -> T.Text
typeToEmote "FIRE" = "<:fire:948981234443370519>"
typeToEmote "GRASS" = "<:grass:948981182337519676>"
typeToEmote "WATER" = "<:water:948980979979157515>"
typeToEmote "ELECTRIC" = "<:electric:948981342257942548>"
typeToEmote "FLYING" = "<:flying:948981202625368065>"
typeToEmote "GROUND" = "<:ground:948981128369410119>"
typeToEmote "STEEL" = "<:steel:948980999834959883>"
typeToEmote "FAIRY" = "<:fairy:948981308644802620>"
typeToEmote "DARK" = "<:dark:948981409144528907>"
typeToEmote "GHOST" = "<:ghost:948981159566667836>"
typeToEmote "FIGHTING" = "<:fighting:948981259915382865>"
typeToEmote "NORMAL" = "<:normal:948982220582957176>"
typeToEmote "ROCK" = "<:rock:948982609906630776>"
typeToEmote "POISON" = "<:poison:948981082068488202>"
typeToEmote "BUG" = "<:bug:948981434016735242>"
typeToEmote "ICE" = "<:ice:948981105774718977>"
typeToEmote "DRAGON" = "<:dragon:948981381407604797>"
typeToEmote "PSYCHIC" = "<:psychic:948981051227799552>"
typeToEmote _ = "<:normal:948982220582957176>"

sortMoves :: [Move] -> [Move]
sortMoves = sort


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
    "torment",
    "gravity",
    "wonder-room",
    "roar",
    "whirlwind",
    "ally-switch",
    "block",
    "imprison",
    "transform",
    "magnet-rise"
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
    "encore",
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
    "teeter-dance"
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

pikachuUSUMForms :: [T.Text]
pikachuUSUMForms = ["pikachu-cosplay", "pikachu-belle", "pikachu-libre", "pikachu-phd", "pikachu-pop-star", "pikachu-rock-star"]

notGen8 :: [Int]
notGen8 =
  [ 13,
    14,
    15,
    16,
    17,
    18,
    19,
    20,
    21,
    22,
    23,
    24,
    46,
    47,
    48,
    49,
    56,
    57,
    69,
    70,
    71,
    74,
    75,
    76,
    84,
    85,
    86,
    87,
    88,
    89,
    96,
    97,
    100,
    101,
    152,
    153,
    154,
    155,
    156,
    157,
    158,
    159,
    160,
    161,
    162,
    165,
    166,
    167,
    168,
    179,
    180,
    181,
    187,
    188,
    189,
    190,
    191,
    192,
    193,
    198,
    200,
    201,
    203,
    204,
    205,
    207,
    209,
    210,
    216,
    217,
    218,
    219,
    228,
    229,
    231,
    232,
    234,
    235,
    261,
    262,
    265,
    266,
    267,
    268,
    269,
    276,
    277,
    283,
    284,
    285,
    286,
    287,
    288,
    289,
    296,
    297,
    299,
    300,
    301,
    307,
    308,
    311,
    312,
    313,
    314,
    316,
    317,
    322,
    323,
    325,
    326,
    327,
    331,
    332,
    335,
    336,
    351,
    352,
    353,
    354,
    357,
    358,
    366,
    367,
    368,
    370,
    386,
    387,
    388,
    389,
    390,
    391,
    392,
    393,
    394,
    395,
    396,
    397,
    398,
    399,
    400,
    401,
    402,
    408,
    409,
    410,
    411,
    412,
    413,
    414,
    417,
    418,
    419,
    424,
    429,
    430,
    431,
    432,
    433,
    441,
    455,
    456,
    457,
    469,
    472,
    476,
    489,
    490,
    491,
    492,
    493,
    495,
    496,
    497,
    498,
    499,
    500,
    501,
    502,
    503,
    504,
    505,
    511,
    512,
    513,
    514,
    515,
    516,
    522,
    523,
    540,
    541,
    542,
    580,
    581,
    585,
    586,
    594,
    602,
    603,
    604,
    648,
    650,
    651,
    652,
    653,
    654,
    655,
    656,
    657,
    658,
    664,
    665,
    666,
    667,
    668,
    669,
    670,
    671,
    672,
    673,
    676,
    720,
    731,
    732,
    733,
    734,
    735,
    739,
    740,
    741,
    774,
    775,
    779
  ]

multiTargetMoves :: [T.Text]
multiTargetMoves =
  [ "boomburst",
    "brutalswing",
    "bulldoze",
    "discharge",
    "earthquake",
    "explosion",
    "lavaplume",
    "magnitude",
    "mindblown",
    "parabolicharge",
    "petalblizzard",
    "searingshot",
    "selfdestruct",
    "sludgewave",
    "sparklingaria",
    "surf",
    "synchronoise",
    "acid",
    "aircutter",
    "astralbarrage",
    "blizzard",
    "breakingswipe",
    "bubble",
    "burningjealousy",
    "clangingscales",
    "clangoroussoulblaze",
    "coreenforcer",
    "dazzlinggleam",
    "diamondstorm",
    "disarmingvoice",
    "dragonenergy",
    "electroweb",
    "eruption",
    "expandingforce",
    "fierywrath",
    "glaciallance",
    "glaciate",
    "heatwave",
    "hypervoice",
    "icywind",
    "incinerate",
    "landswrath",
    "muddywater",
    "originpulse",
    "overdrive",
    "poisongas",
    "powdersnow",
    "precipiceblades",
    "razorleaf",
    "razorwind",
    "relicsong",
    "rockslide",
    "shelltrap",
    "snarl",
    "splishysplash",
    "strugglebug",
    "swift",
    "thousandarrows",
    "thousandwaves",
    "twister",
    "venomdrench",
    "waterspout"
  ]

movesThatIgnoreAbilities :: [T.Text]
movesThatIgnoreAbilities =
  [ "gmaxdrumsolo",
    "gmaxfireball",
    "gmaxhydrosnipe",
    "lightthatburnsthesky",
    "menacingmoonrazemaelstrom",
    "moongeistbeam",
    "photongeyser",
    "searingsunrazesmash",
    "sunsteelstrike"
  ]

abilityIgnoringAbilities :: [T.Text]
abilityIgnoringAbilities = ["moldbreaker", "teravolt", "turboblaze"]

unknockableItems :: [T.Text]
unknockableItems =
  [ "buginiumz",
    "darkiniumz",
    "dragoniumz",
    "electriumz",
    "fairiumz",
    "fightiniumz",
    "firiumz",
    "flyiniumz",
    "ghostiumz",
    "grassiumz",
    "groundiumz",
    "iciumz",
    "normaliumz",
    "poisoniumz",
    "psychiumz",
    "rockiumz",
    "steeliumz",
    "wateriumz",
    "alolaraichiumz",
    "decidiumz",
    "eeviumz",
    "inciniumz",
    "kommoniumz",
    "lunaliumz",
    "lycaniumz",
    "marshadiumz",
    "mewniumz",
    "mimikiumz",
    "pikaniumz",
    "pikashuniumz",
    "primariumz",
    "snorliumz",
    "solganiumz",
    "tapuniumz",
    "ultranecroziumz",
    "abomasite",
    "absolite",
    "aerodactylite",
    "aggronite",
    "alakazite",
    "altarianite",
    "ampharosite",
    "audinite",
    "banettite",
    "beedrillite",
    "blastoisinite",
    "blazikenite",
    "cameruptite",
    "charizarditex",
    "charizarditey",
    "diancite",
    "galladite",
    "garchompite",
    "gardevoirite",
    "gengarite",
    "glalitite",
    "gyaradosite",
    "heracronite",
    "houndoominite",
    "kangaskhanite",
    "latiasite",
    "latiosite",
    "lopunnite",
    "lucarionite",
    "manectite",
    "mawilite",
    "medichamite",
    "metagrossite",
    "mewtwonitex",
    "mewtwonitey",
    "pidgeotite",
    "pinsirite",
    "sablenite",
    "salamencite",
    "sceptilite",
    "scizorite",
    "sharpedonite",
    "slowbronite",
    "steelixite",
    "swampertite",
    "tyranitarite",
    "venusaurite"
  ]

recoilBlockingAbilities :: [T.Text]
recoilBlockingAbilities =
  [ "rockhead",
    "magicguard"
  ]

toId :: T.Text -> T.Text
toId = T.replace " " "" . T.replace "-" "" . T.toLower

allTypes :: [Type]
allTypes = [NORMAL, DRAGON, DARK, FAIRY, BUG, PSYCHIC, GHOST, POISON, WATER, GRASS, FIRE, ELECTRIC, FLYING, FIGHTING, ICE, ROCK, STEEL, GROUND]