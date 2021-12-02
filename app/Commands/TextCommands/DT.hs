{-# LANGUAGE RecordWildCards #-}

module Commands.TextCommands.DT (dtCommand) where

import Commands.Parsers
import Commands.Types
import Commands.Utility
import Control.Monad
import Control.Monad.Trans
import Data.Either
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Text hiding (length, map, null)
import Database.PostgreSQL.Simple
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Pokemon.DBConversion
import Pokemon.Functions (notGen8, pikachuUSUMForms)
import Pokemon.Nature
import Pokemon.Types
import PokemonDB.Connection (getDbConnEnv)
import qualified PokemonDB.Queries as Q
import PokemonDB.Types
import Text.Parsec

dtCommand :: Command
dtCommand = Com "ldt (item/pokemon/move/nature/ability)" (TextCommand returnDT)

returnDT :: Message -> DiscordHandler ()
returnDT m = do
  let (t, opts) = parseOptions (toLower (messageText m))
      dt = parse dtP "parse DT" t
  ifElse (isLeft dt) (nonValidDt m) (handleDt (extractRight dt) opts m)

handleDt :: Text -> M.Map Text Text -> Message -> DiscordHandler ()
handleDt dt' opts m = do
  let dt = toId dt'
      nature = getNature dt
  ifElse (isNothing nature) (pokemonDb (handleDt' dt opts) m) (sendMessage $ R.CreateMessageEmbed (messageChannel m) "" (createNatureEmbed (fromJust nature)))

handleDt' :: Text -> M.Map Text Text -> Connection -> Message -> DiscordHandler ()
handleDt' dt opts con m = do
  dt' <- lift $ Q.getData con dt
  lift $ close con
  ifElse (isNothing dt') (dtNotFound m) (dtFound (toDTType . fromJust $ dt') opts m)

dtFound :: DTType -> M.Map Text Text -> Message -> DiscordHandler ()
dtFound (DtPokemon mon) opts m = sendMessage (R.CreateMessageEmbed (messageChannel m) "" (createPokemonEmbed mon opts))
dtFound (DtAbility ability) opts m = sendMessage (R.CreateMessageEmbed (messageChannel m) "" (createAbilityEmbed ability))
dtFound (DtItem item) opts m = sendMessage (R.CreateMessageEmbed (messageChannel m) "" (createItemEmbed item))
dtFound (DtMove move) opts m = sendMessage (R.CreateMessageEmbed (messageChannel m) "" (createMoveEmbed move))
dtFound dt _ m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ ", " ++ show dt))

dtNotFound :: Message -> DiscordHandler ()
dtNotFound m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", couldn't find the data you were looking for.")

nonValidDt :: Message -> DiscordHandler ()
nonValidDt m = void . restCall $ R.CreateMessage (messageChannel m) "Usage: ldt (item/pokemon/move/nature/ability)"

createPokemonEmbed :: Pokemon -> M.Map Text Text -> CreateEmbed
createPokemonEmbed mon opts =
  def
    { createEmbedImage = Just $ CreateEmbedImageUrl (if gif then getGifUrl mon opts else getImageUrl mon opts),
      createEmbedTitle = L.foldl append "" [pName mon, " (", pack . show $ pNum mon, ")"],
      createEmbedColor = sideColor,
      createEmbedFields =
        [ EmbedField "Stats" (getStats mon) (Just True),
          EmbedField "Abilities" (getAbilities mon) (Just True),
          EmbedField "---------------------------------------------------" "**---------------------------------------------------**" Nothing,
          EmbedField "Typing" (getTyping mon) (Just True),
          EmbedField "Misc" (getMisc mon) (Just True)
        ]
    }
  where
    gif = "gif" `M.member` opts

createNatureEmbed :: Nature -> CreateEmbed
createNatureEmbed (Nature name NEUTRAL NEUTRAL) =
  def
    { createEmbedTitle = pack name,
      createEmbedColor = sideColor,
      createEmbedDescription = "No effect on stats!"
    }
createNatureEmbed (Nature name pos neg) =
  def
    { createEmbedTitle = pack name,
      createEmbedColor = sideColor,
      createEmbedDescription = L.foldl append "10% increase for " [(pack . show) pos, ", 10% decrease for ", (pack . show) neg, "!"]
    }

createAbilityEmbed :: Ability -> CreateEmbed
createAbilityEmbed (Ability name (Just desc)) =
  def
    { createEmbedTitle = name,
      createEmbedColor = sideColor,
      createEmbedDescription = desc
    }
createAbilityEmbed (Ability name Nothing) = def {createEmbedTitle = name, createEmbedDescription = "No description for this ability yet!"}

createItemEmbed :: Item -> CreateEmbed
createItemEmbed (Item name (Just desc) _ flingBp _ _ _) =
  def
    { createEmbedTitle = L.foldl append name [" (flingBP=", pack . show $ flingBp, ")"],
      createEmbedThumbnail = (Just . CreateEmbedImageUrl . getItemUrl) name,
      createEmbedColor = sideColor,
      createEmbedDescription = desc
    }
createItemEmbed (Item name Nothing _ flingBp _ _ _) = def {createEmbedTitle = L.foldl append name [" (flingBP=", pack . show $ flingBp, ")"], createEmbedDescription = "No description for this item yet!"}

getItemUrl :: Text -> Text
getItemUrl name = L.foldl append "https://www.serebii.net/itemdex/sprites/pgl/" [toId name,".png"]

createMoveEmbed :: Move -> CreateEmbed
createMoveEmbed move =
  def
    { createEmbedTitle = mName move,
      createEmbedColor = sideColor,
      createEmbedDescription = if isNothing (mDescription move) then "" else fromJust . mDescription $ move,
      createEmbedFields = [EmbedField "Stats" (getStatsText move) (Just True), EmbedField "Flags" (if null (mFlags move) then "No flags" else intercalate "\n" (mFlags move)) (Just True)]
    }

getStatsText :: Move -> Text
getStatsText move = intercalate "\n" [typeText, dmgClassText, bpText, accuracyText]
  where
    bpText = if isNothing (mBp move) then "**bp**: 0" else append "**bp:** " (pack . show . fromJust . mBp $ move)
    accuracyText = if isNothing (mAccuracy move) then "**accuracy**: never misses" else append "**accuracy:** " (pack . show . fromJust . mAccuracy $ move)
    typeText = pack . show . mTipe $ move
    dmgClassText = pack . show . mDClass $ move

discordShow :: BaseStat -> Text
discordShow (BaseStat stat val) = L.foldl append "" ["**", (pack . show) stat, "**: ", (pack . show) val]

getMisc :: Pokemon -> Text
getMisc mon = L.foldl append "**" ["Weight:** ", pack . show $ pWeight mon, "\n", "**Colour:** ", pColour mon]

getStats :: Pokemon -> Text
getStats mon = intercalate "\n" (map discordShow bs)
  where
    bs = baseStats mon

getTyping :: Pokemon -> Text
getTyping mon = intercalate "\n" (map (pack . show) . pTyping $ mon)

getAbilities :: Pokemon -> Text
getAbilities mon = intercalate "\n" (abilities mon)

getGifUrl :: Pokemon -> M.Map Text Text -> Text
getGifUrl Pokemon {..} opts = L.foldl append base [toLower pName, ".gif"]
  where
    isShiny = M.member "shiny" opts
    base = if isShiny then "http://play.pokemonshowdown.com/sprites/ani-shiny/" else "http://play.pokemonshowdown.com/sprites/ani/"

getImageUrl :: Pokemon -> M.Map Text Text -> Text
getImageUrl Pokemon {..} opts = L.foldl append "https://www.serebii.net/" image
  where
    isMega = "mega" `isInfixOf` toLower pName || "primal" `isInfixOf` toLower pName
    isGmax = "gmax" `isInfixOf` toLower pName
    isGalarian = "galar" `isInfixOf` toLower pName
    isAlolan = "alola" `isInfixOf` toLower pName
    isGen8 = pNum `notElem` notGen8 && toLower pName `notElem` pikachuUSUMForms
    isShiny = M.member "shiny" opts
    base = getBase isMega isGen8 isShiny
    number = justifyRight 3 '0' . pack . show $ pNum
    season = M.lookup "season" opts
    form = M.lookup "form" opts
    additionalArg = getAdditional (toLower pName) isMega isGmax isAlolan isGalarian season form
    imageType = ".png"
    image = [base, number, additionalArg, imageType]
    -- GET THE CORRECT BASE USING KNOWLEDGE OF WHICH MONS ARE IN GEN8 AND WHICH AREN'T
    getBase True _ False = "sunmoon/pokemon/"
    getBase False False False = "sunmoon/pokemon/"
    getBase True _ True = "Shiny/SM/"
    getBase False False True = "Shiny/SM/"
    getBase False True True = "Shiny/SWSH/"
    getBase _ _ _ = "swordshield/pokemon/"

-- FOR SOME MONS EXTRA QUALIFIERS ARE NEEDED, EXCEPTIONS ARE PATTERN MATCHED
getAdditional "deoxys-speed" _ _ _ _ _ _ = "-s"
getAdditional "deoxys-defense" _ _ _ _ _ _ = "-d"
getAdditional "deoxys-attack" _ _ _ _ _ _ = "-a"
getAdditional "castform-rainy" _ _ _ _ _ _ = "-r"
getAdditional "castform-snowy" _ _ _ _ _ _ = "-i"
getAdditional "castform-sunny" _ _ _ _ _ _ = "-s"
getAdditional "burmy" _ _ _ _ _ (Just form) = case form of
  "sandy" -> "-c"
  "trash" -> "-t"
  other -> ""
getAdditional "wormadam-sandy" _ _ _ _ _ _ = "-c"
getAdditional "wormadam-trash" _ _ _ _ _ _ = "-s"
getAdditional "deerling" _ _ _ _ (Just s') _ = case s' of
  "summer" -> "-s"
  "autumn" -> "-a"
  "winter" -> "-w"
  other -> ""
getAdditional "sawsbuck" _ _ _ _ (Just s') _ = case s' of
  "summer" -> "-s"
  "autumn" -> "-a"
  "winter" -> "-w"
  other -> ""
getAdditional "calyrex-shadow" _ _ _ _ _ _ = "-s"
getAdditional "calyrex-ice" _ _ _ _ _ _ = "-i"
getAdditional "necrozma-dusk-mane" _ _ _ _ _ _ = "-dm"
getAdditional "necrozma-dawn-wings" _ _ _ _ _ _ = "-dw"
getAdditional "necrozma-ultra" _ _ _ _ _ _ = "-u"
getAdditional "kyurem-white" _ _ _ _ _ _ = "-w"
getAdditional "kyurem-black" _ _ _ _ _ _ = "-b"
getAdditional "kyogre-primal" _ _ _ _ _ _ = "-p"
getAdditional "groudon-primal" _ _ _ _ _ _ = "-p"
getAdditional "giratina-origin" _ _ _ _ _ _ = "-o"
getAdditional "landorus-therian" _ _ _ _ _ _ = "-t"
getAdditional "thundurus-therian" _ _ _ _ _ _ = "-t"
getAdditional "tornadus-therian" _ _ _ _ _ _ = "-t"
getAdditional "keldeo-resolute" _ _ _ _ _ _ = "-r"
getAdditional "zarude-dada" _ _ _ _ _ _ = "-d"
getAdditional "meloetta-pirouette" _ _ _ _ _ _ = "-s"
getAdditional "urshifu-rapid-strike-gmax" _ _ _ _ _ _ = "-rgi"
getAdditional "urshifu-rapid-strike" _ _ _ _ _ _ = "-r"
getAdditional "eternatus-eternamax" _ _ _ _ _ _ = "-e"
getAdditional "zygarde-10%" _ _ _ _ _ _ = "-10"
getAdditional "zygarde-complete" _ _ _ _ _ _ = "-c"
getAdditional "genesect-burn" _ _ _ _ _ _ = "-f"
getAdditional "genesect-chill" _ _ _ _ _ _ = "-i"
getAdditional "genesect-douse" _ _ _ _ _ _ = "-w"
getAdditional "genesect-shock" _ _ _ _ _ _ = "-e"
getAdditional "hoopa-unbound" _ _ _ _ _ _ = "-u"
getAdditional "gourgeist-small" _ _ _ _ _ _ = "-s"
getAdditional "gourgeist-super" _ _ _ _ _ _ = "-h"
getAdditional "gourgeist-large" _ _ _ _ _ _ = "-l"
getAdditional "pumpkaboo-small" _ _ _ _ _ _ = "-s"
getAdditional "pumpkaboo-super" _ _ _ _ _ _ = "-h"
getAdditional "pumpkaboo-large" _ _ _ _ _ _ = "-l"
getAdditional "lycanroc-dusk" _ _ _ _ _ _ = "-d"
getAdditional "lycanroc-midnight" _ _ _ _ _ _ = "-m"
getAdditional "meowstic-f" _ _ _ _ _ _ = "-f"
getAdditional "indeedee-f" _ _ _ _ _ _ = "-f"
getAdditional "mimikyu-busted" _ _ _ _ _ _ = "-b"
getAdditional "zacian-crowned" _ _ _ _ _ _ = "-c"
getAdditional "zamazenta-crowned" _ _ _ _ _ _ = "-c"
getAdditional "greninja-ash" _ _ _ _ _ _ = "-a"
getAdditional "darmanitan-galar-zen" _ _ _ _ _ _ = "-gz"
getAdditional "darmanitan-zen" _ _ _ _ _ _ = "-z"
getAdditional "shaymin-sky" _ _ _ _ _ _ = "-s"
getAdditional "pikachu-original" _ _ _ _ _ _ = "-o"
getAdditional "pikachu-alola" _ _ _ _ _ _ = "-a"
getAdditional "pikachu-sinnoh" _ _ _ _ _ _ = "-s"
getAdditional "pikachu-unova" _ _ _ _ _ _ = "-u"
getAdditional "pikachu-world" _ _ _ _ _ _ = "-w"
getAdditional "pikachu-hoenn" _ _ _ _ _ _ = "-h"
getAdditional "pikachu-kalos" _ _ _ _ _ _ = "-k"
getAdditional "pikachu-partner" _ _ _ _ _ _ = "-p"
getAdditional "pikachu-belle" _ _ _ _ _ _ = "-b"
getAdditional "pikachu-phd" _ _ _ _ _ _ = "-phd"
getAdditional "pikachu-libre" _ _ _ _ _ _ = "-l"
getAdditional "pikachu-pop-star" _ _ _ _ _ _ = "-ps"
getAdditional "pikachu-cosplay" _ _ _ _ _ _ = "-c"
getAdditional "pikachu-rock-star" _ _ _ _ _ _ = "-r"
getAdditional "oricorio-pa'u" _ _ _ _ _ _ = "-pau"
getAdditional "oricorio-pom-pom" _ _ _ _ _ _ = "-p"
getAdditional "oricorio-sensu" _ _ _ _ _ _ = "-s"
getAdditional "pikachu" _ _ _ _ _ (Just "female") = "-f"
getAdditional "murkrow" _ _ _ _ _ (Just "female") = "-f"
getAdditional "numel" _ _ _ _ _ (Just "female") = "-f"
getAdditional "nuzleaf" _ _ _ _ _ (Just "female") = "-f"
getAdditional "octillery" _ _ _ _ _ (Just "female") = "-f"
getAdditional "alcremie" _ _ _ _ _ (Just form) = case form of
  "vanillacream berry" -> "-berry"
  "vanillacream love" -> "-love"
  "vanillacream star" -> "-star"
  "vanillacream clover" -> "-clover"
  "vanillacream flower" -> "-flower"
  "vanillacream ribbon" -> "-ribbon"
  "rubycream" -> "-rc"
  "rubycream berry" -> "-rcberry"
  "rubycream love" -> "-rclove"
  "rubycream star" -> "-rcstar"
  "rubycream clover" -> "-rcclover"
  "rubycream flower" -> "-rcflower"
  "rubycream ribbon" -> "-rcribbon"
  "matchacream" -> "-mac"
  "matchacream berry" -> "-macberry"
  "matchacream love" -> "-maclove"
  "matchacream star" -> "-macstar"
  "matchacream clover" -> "-macclover"
  "matchacream flower" -> "-macflower"
  "matchacream ribbon" -> "-macribbon"
  "mintcream" -> "-mic"
  "mintcream berry" -> "-micberry"
  "mintcream love" -> "-miclove"
  "mintcream star" -> "-micstar"
  "mintcream clover" -> "-micclover"
  "mintcream flower" -> "-flowermic"
  "mintcream ribbon" -> "-micribbon"
  "lemoncream" -> "-lc"
  "lemoncream berry" -> "-lcberry"
  "lemoncream love" -> "-lclove"
  "lemoncream star" -> "-lcstar"
  "lemoncream clover" -> "-lcclover"
  "lemoncream flower" -> "-lcflower"
  "lemoncream ribbon" -> "-lcribbon"
  "saltedcream" -> "-sc"
  "saltedcream berry" -> "-scberry"
  "saltedcream love" -> "-sclove"
  "saltedcream star" -> "-scstar"
  "saltedcream clover" -> "-scclover"
  "saltedcream flower" -> "-scflower"
  "saltedcream ribbon" -> "-scribbon"
  "rubyswirl" -> "-rs"
  "rubyswirl berry" -> "-rsberry"
  "rubyswirl love" -> "-rslove"
  "rubyswirl star" -> "-rsstar"
  "rubyswirl clover" -> "-rsclover"
  "rubyswirl flower" -> "-rsflower"
  "rubyswirl ribbon" -> "-rsribbon"
  "caramelswirl" -> "-cs"
  "caramelswirl berry" -> "-csberry"
  "caramelswirl love" -> "-cslove"
  "caramelswirl star" -> "-csstar"
  "caramelswirl clover" -> "-csclover"
  "caramelswirl flower" -> "-csflower"
  "caramelswirl ribbon" -> "-csribbon"
  "rainbowswirl" -> "-ras"
  "rainbowswirl berry" -> "-rasberry"
  "rainbowswirl love" -> "-raslove"
  "rainbowswirl star" -> "-rasstar"
  "rainbowswirl clover" -> "-rasclover"
  "rainbowswirl flower" -> "-rasflower"
  "rainbowswirl ribbon" -> "-rasribbon"
  other -> ""
getAdditional "florges" _ _ _ _ _ (Just form) = case form of
  "yellow" -> "-y"
  "orange" -> "-o"
  "blue" -> "-b"
  "white" -> "-w"
  other -> ""
getAdditional "floette" _ _ _ _ _ (Just form) = case form of
  "yellow" -> "-y"
  "orange" -> "-o"
  "blue" -> "-b"
  "white" -> "-w"
  other -> ""
getAdditional "floette-eternal" _ _ _ _ _ _ = "-e"
getAdditional "flabebe" _ _ _ _ _ (Just form) = case form of
  "yellow" -> "-y"
  "orange" -> "-o"
  "blue" -> "-b"
  "white" -> "-w"
  other -> ""
getAdditional "furfrou" _ _ _ _ _ (Just form) = case form of
  "heart" -> "-h"
  "star" -> "-s"
  "diamond" -> "-d"
  "deputante" -> "-de"
  "matron" -> "-m"
  "dandy" -> "-da"
  "la reine" -> "-l"
  "kabuki" -> "-k"
  "pharaoh" -> "-p"
  other -> ""
getAdditional "minior" _ _ _ _ _ (Just form) = case form of
  "yellow" -> "-y"
  "orange" -> "-o"
  "blue" -> "-b"
  "violet" -> "-v"
  "indigo" -> "indigo"
  "red" -> "red"
  "green" -> "green"
  other -> ""
getAdditional "vivillon-fancy" _ _ _ _ _ _ = "-f"
getAdditional "vivillon-pokeball" _ _ _ _ _ _ = "-pb"
getAdditional "vivillon" _ _ _ _ _ (Just form) = case form of
  "polar" -> "-p"
  "tundra" -> "-t"
  "continental" -> "-c"
  "garden" -> "-g"
  "elegant" -> "-e"
  "icysnow" -> "-i"
  "modern" -> "-mo"
  "marine" -> "-ma"
  "archipelago" -> "-a"
  "highplains" -> "-h"
  "sandstorm" -> "-s"
  "river" -> "-r"
  "monsoon" -> "-mon"
  "savanna" -> "-sa"
  "sun" -> "-su"
  "ocean" -> "-o"
  "jungle" -> "-j"
  other -> ""
getAdditional "wishiwashi-school" _ _ _ _ _ _ = "-s"
getAdditional "aegislash-blade" _ _ _ _ _ _ = "-b"
getAdditional "cramorant-gulping" _ _ _ _ _ _ = "-gu"
getAdditional "cramorant-gorging" _ _ _ _ _ _ = "-go"
getAdditional "basculin-blue-striped" _ _ _ _ _ _ = "-b"
getAdditional "cherrim-sunshine" _ _ _ _ _ _ = "-s"
getAdditional "morpeko-hangry" _ _ _ _ _ _ = "-h"
getAdditional "morpeko-noice" _ _ _ _ _ _ = "-n"
getAdditional "shellos" _ _ _ _ _ (Just "east") = "-e"
getAdditional "gastrodon" _ _ _ _ _ (Just "east") = "-e"
getAdditional "rotom-wash" _ _ _ _ _ _ = "-w"
getAdditional "rotom-fan" _ _ _ _ _ _ = "-s"
getAdditional "rotom-heat" _ _ _ _ _ _ = "-h"
getAdditional "rotom-frost" _ _ _ _ _ _ = "-f"
getAdditional "rotom-mow" _ _ _ _ _ _ = "-m"
getAdditional "arceus-bug" _ _ _ _ _ _ = "-bug"
getAdditional "arceus-dark" _ _ _ _ _ _ = "-dark"
getAdditional "arceus-dragon" _ _ _ _ _ _ = "-dragon"
getAdditional "arceus-electric" _ _ _ _ _ _ = "-electric"
getAdditional "arceus-fairy" _ _ _ _ _ _ = "-fairy"
getAdditional "arceus-fighting" _ _ _ _ _ _ = "-fighting"
getAdditional "arceus-fire" _ _ _ _ _ _ = "-fire"
getAdditional "arceus-flying" _ _ _ _ _ _ = "-flying"
getAdditional "arceus-ghost" _ _ _ _ _ _ = "-ghost"
getAdditional "arceus-grass" _ _ _ _ _ _ = "-grass"
getAdditional "arceus-ground" _ _ _ _ _ _ = "-ground"
getAdditional "arceus-ice" _ _ _ _ _ _ = "-ice"
getAdditional "arceus-poison" _ _ _ _ _ _ = "-poison"
getAdditional "arceus-psychic" _ _ _ _ _ _ = "-psychic"
getAdditional "arceus-rock" _ _ _ _ _ _ = "-rock"
getAdditional "arceus-steel" _ _ _ _ _ _ = "-steel"
getAdditional "arceus-water" _ _ _ _ _ _ = "-water"
getAdditional "silvally-bug" _ _ _ _ _ _ = "-bug"
getAdditional "silvally-dark" _ _ _ _ _ _ = "-dark"
getAdditional "silvally-dragon" _ _ _ _ _ _ = "-dragon"
getAdditional "silvally-electric" _ _ _ _ _ _ = "-electric"
getAdditional "silvally-fairy" _ _ _ _ _ _ = "-fairy"
getAdditional "silvally-fighting" _ _ _ _ _ _ = "-fighting"
getAdditional "silvally-fire" _ _ _ _ _ _ = "-fire"
getAdditional "silvally-flying" _ _ _ _ _ _ = "-flying"
getAdditional "silvally-ghost" _ _ _ _ _ _ = "-ghost"
getAdditional "silvally-grass" _ _ _ _ _ _ = "-grass"
getAdditional "silvally-ground" _ _ _ _ _ _ = "-ground"
getAdditional "silvally-ice" _ _ _ _ _ _ = "-ice"
getAdditional "silvally-poison" _ _ _ _ _ _ = "-poison"
getAdditional "silvally-psychic" _ _ _ _ _ _ = "-psychic"
getAdditional "silvally-rock" _ _ _ _ _ _ = "-rock"
getAdditional "silvally-steel" _ _ _ _ _ _ = "-steel"
getAdditional "silvally-water" _ _ _ _ _ _ = "-water"
getAdditional "charizard-mega-x" _ _ _ _ _ _ = "-mx"
getAdditional "charizard-mega-y" _ _ _ _ _ _ = "-my"
getAdditional "mewtwo-mega-x" _ _ _ _ _ _ = "-mx"
getAdditional "mewtwo-mega-y" _ _ _ _ _ _ = "-my"
getAdditional _ False True _ _ _ _ = "-gi"
getAdditional _ _ _ True _ _ _ = "-a"
getAdditional _ _ _ _ True _ _ = "-g"
getAdditional _ True _ _ _ _ _ = "-m"
getAdditional _ _ _ _ _ _ _ = ""
