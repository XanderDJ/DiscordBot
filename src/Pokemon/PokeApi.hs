{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Pokemon.PokeApi
  ( getPokemon,
    getPokemonNoMoves,
    getItem,
    getAbility,
    getMove,
    getNature,
    getDt,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    decode,
    (.:),
    (.:?),
  )
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Text as T
import Network.HTTP.Client
  ( ManagerSettings,
    Response (responseBody, responseStatus),
    httpLbs,
    newManager,
    parseRequest_,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (status404)
import Pokemon.Functions (getStat, toId)
import Pokemon.Nature (getNature)
import Pokemon.Types (Ability (..), AttackType (..), BaseStat (..), DTType (..), Item (..), Move (Move), Pokemon (Pokemon, pMoves))


-- | Manager settings for tls connections
settings :: ManagerSettings
settings = tlsManagerSettings

-- | Implement the /dt command from showdown with pokéapi. Should use DTCache in the future to store looked up items in a cache
getDt :: String -> IO (Maybe DTType)
getDt name = do
  let name' = (intercalate "-" . words) name
      nature = getNature (T.pack name')
  if isJust nature
    then return $ Just $ DtNature (fromJust nature)
    else do
      let allUrls = tryAllBases name
          allRequests = map parseRequest_ allUrls
      manager <- newManager settings
      responses <- mapConcurrently (`httpLbs` manager) allRequests
      let bodies = map responseBody responses
          dts = fromBS4 bodies
          dt = getDts dts
      return dt

-- | Get a pokemon from pokéapi.
getPokemon :: String -> IO (Maybe Pokemon)
getPokemon name = do
  let base = pokemonBase ++ name
  manager <- newManager settings
  response <- httpLbs (parseRequest_ base) manager
  if checkFound response
    then do
      let mon = decode (responseBody response) :: Maybe Pokemon
          mn = decode (responseBody response) :: Maybe MoveNames
      if isJust mn && isJust mon
        then do
          let (MN names) = fromJust mn
              mon' = fromJust mon
          if isJust names
            then do
              let names' = fromJust names
              moves <- mapM getMove names'
              let mon'' = mon' {pMoves = sequence moves}
              return $ Just mon''
            else return mon
        else return mon
    else return Nothing

getPokemonNoMoves :: String -> IO (Either String Pokemon)
getPokemonNoMoves pokemon = do
  let base = pokemonBase ++ pokemon
  getResponse base pokemon

-- | Get an ability from pokéapi.
getAbility :: String -> IO (Either String Ability)
getAbility name = do
  let base = abilityBase ++ name
  getResponse base name

-- | Get an item from pokéapi.
getItem :: String -> IO (Either String Item)
getItem name = do
  let base = itemBase ++ name
  getResponse base name

-- | Get a move from pokéapi.
getMove :: String -> IO (Either String Move)
getMove name = do
  let base = moveBase ++ name
  getResponse base name

-- | Get the names of moves learned by a pokemon
getMoveNames :: String -> IO (Either String MoveNames)
getMoveNames pokemon = do
  let base = pokemonBase ++ pokemon
  getResponse base pokemon

-- | Fetch an API request from the api
getResponse :: FromJSON a => String -> String -> IO (Either String a)
getResponse base name = do
  manager <- newManager settings
  response <- httpLbs (parseRequest_ base) manager
  if checkFound response
    then do
      let resp = decode (responseBody response)
      if isNothing resp then return $ Left base else return $ Right (fromJust resp)
    else return $ Left name

checkFound :: Response ByteString -> Bool
checkFound resp = status404 /= responseStatus resp

-- | Api base for pokéapi, make sure this remains up to date
apiBase :: String
apiBase = "https://pokeapi.co/api/v2/"

pokemonBase :: String
pokemonBase = apiBase ++ "pokemon/"

abilityBase :: String
abilityBase = apiBase ++ "ability/"

moveBase :: String
moveBase = apiBase ++ "move/"

itemBase :: String
itemBase = apiBase ++ "item/"

allBases :: [String]
allBases = [pokemonBase, abilityBase, moveBase, itemBase]

-- | When running a dt command we don't know what the user has given us, so we try all bases
tryAllBases :: String -> [String]
tryAllBases name =
  let name' = (intercalate "-" . words) name
   in map (++ name') allBases


fromBS4 :: [ByteString] -> (Maybe Pokemon, Maybe Ability, Maybe Move, Maybe Item)
fromBS4 [x1, x2, x3, x4] = (decode x1, decode x2, decode x3, decode x4)
fromBS4 _ = (Nothing, Nothing, Nothing, Nothing)

getDts :: (Maybe Pokemon, Maybe Ability, Maybe Move, Maybe Item) -> Maybe DTType
getDts (Just dt, Nothing, Nothing, Nothing) = Just $ DtPokemon dt
getDts (Nothing, Just dt, Nothing, Nothing) = Just $ DtAbility dt
getDts (Nothing, Nothing, Just dt, Nothing) = Just $ DtMove dt
getDts (Nothing, Nothing, Nothing, Just dt) = Just $ DtItem dt
getDts _ = Nothing

data AbilityJson = AbilityJson
  { _name :: String,
    isHidden :: Bool
  }

instance FromJSON AbilityJson where
  parseJSON (Object jsn) = do
    hidden <- jsn .: "is_hidden"
    ability <- jsn .: "ability"
    name <- ability .: "name"
    return $ AbilityJson name hidden

instance FromJSON BaseStat where
  parseJSON (Object jsn) = do
    value <- jsn .: "base_stat"
    stat <- jsn .: "stat"
    name <- stat .: "name"
    return $ BaseStat (getStat name) value

instance FromJSON Pokemon where
  parseJSON (Object jsn) = do
    name <- jsn .: "name"
    objs <- jsn .: "types"
    typesObjs <- mapM (.: "type") objs
    types <- mapM (.: "name") typesObjs
    weight <- jsn .: "weight"
    allAbilities <- jsn .: "abilities"
    baseStats <- jsn .: "stats"
    let typing = map read types
        abilities = (map (T.pack . _name) . filter (not . isHidden)) allAbilities
        hiddenAbility = (map (T.pack . _name) . filter isHidden) allAbilities
        ha = if length hiddenAbility == 1 then Just $ head hiddenAbility else Nothing
        abilities' = maybe abilities (: abilities) ha
    return $ Pokemon name 0 typing abilities' baseStats (Right []) "" False (div weight 10)

data EffectEntry = EffectEntry
  { language :: T.Text,
    eDescription :: T.Text
  }

data Effect = Effect (Maybe Int) (Maybe T.Text)

instance FromJSON EffectEntry where
  parseJSON (Object jsn) = EffectEntry <$> ((jsn .: "language") >>= (.: "name")) <*> (jsn .: "effect")

instance FromJSON Ability where
  parseJSON (Object jsn) = do
    name <- jsn .: "name"
    effects <- jsn .: "effect_entries"
    let effects' = filter (\(EffectEntry lang _) -> lang == "en") effects
        effect' = if (not . null) effects then (Just . eDescription . head) effects' else Nothing
    return $ Ability (toId name) name effect'

instance FromJSON Item where
  parseJSON (Object jsn) = do
    name <- jsn .: "name"
    effects <- jsn .: "effect_entries"
    bp <- jsn .: "fling_power"
    let effects' = filter (\(EffectEntry lang _) -> lang == "en") effects
        effect' = if (not . null) effects then (Just . eDescription . head) effects' else Nothing
    return $ Item (toId name) name effect' False bp Nothing Nothing Nothing

instance FromJSON Move where
  parseJSON (Object jsn) = do
    name <- jsn .: "name"
    acc <- jsn .:? "accuracy"
    obj <- jsn .: "damage_class"
    dClass <- obj .: "name"
    power <- jsn .:? "power"
    effects <- jsn .: "effect_entries"
    chance <- jsn .:? "effect_chance"
    typesObj <- jsn .: "type"
    tipe <- typesObj .: "name"
    let effects' = filter (\(EffectEntry lang _) -> lang == "en") effects
        effect' = if (not . null) effects then (Just . eDescription . head) effects' else Nothing
        mType = read tipe
        effect'' = Effect chance effect'
    return $ Move name mType (toDClass dClass) power 0 acc (getCompleteDescription effect'') []
    where
      toDClass :: String -> AttackType
      toDClass "special" = SPECIAL
      toDClass "physical" = PHYSICAL
      toDClass o = OTHER

newtype MoveNames = MN (Maybe [String]) deriving (Show)

instance FromJSON MoveNames where
  parseJSON (Object jsn) = do
    moveTuple <- jsn .:? "moves"
    if isJust moveTuple
      then do
        let movesTuple = fromJust moveTuple
        moves <- mapM (.: "move") movesTuple
        moveNames <- mapM (.: "name") moves
        if null moves then return $ MN Nothing else return $ MN $ Just moveNames
      else error "Couldn't find moves key in requested object."

getCompleteDescription :: Effect -> Maybe T.Text
getCompleteDescription (Effect _ Nothing) = Nothing
getCompleteDescription (Effect Nothing (Just desc)) = Just desc
getCompleteDescription (Effect (Just x) (Just desc)) = Just newDesc
  where
    valueText = T.pack $ show x
    newDesc = T.replace "$effect_chance" valueText desc
