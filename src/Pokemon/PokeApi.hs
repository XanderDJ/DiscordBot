
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

import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    decode,
    (.:),
    (.:?),
  )
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
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
import Pokemon.Types
  ( Ability (..),
    BaseStat (..),
    DTType (..),
    Item (..),
    Move (Move),
    Pokemon (..),
  )
import Pokemon.Functions (getStat)
import Pokemon.Nature (getNature)

-- | In memory store of names to dt types, when implemented should lessen the load on pokéapi
type DTCache = M.Map String DTType

-- | Manager settings for tls connections
settings :: ManagerSettings
settings = tlsManagerSettings

-- | Implement the /dt command from showdown with pokéapi. Should use DTCache in the future to store looked up items in a cache
getDt :: String -> IO (Maybe DTType)
getDt name = do
  let name' = (intercalate "-" . words) name
      nature = getNature name'
  if isJust nature
    then return $ Just $ DtNature (fromJust nature)
    else do
      let allUrls = tryAllBases name
          allRequests = map4 parseRequest_ allUrls
      manager <- newManager settings
      responses <- mapM4 (`httpLbs` manager) allRequests
      let bodies = map4 responseBody responses
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

getPokemonNoMoves :: String -> IO (Maybe Pokemon)
getPokemonNoMoves pokemon = do
  let base = pokemonBase ++ pokemon
  getResponse base

-- | Get an ability from pokéapi.
getAbility :: String -> IO (Maybe Ability)
getAbility name = do
  let base = abilityBase ++ name
  getResponse base

-- | Get an item from pokéapi.
getItem :: String -> IO (Maybe Item)
getItem name = do
  let base = itemBase ++ name
  getResponse base

-- | Get a move from pokéapi.
getMove :: String -> IO (Maybe Move)
getMove name = do
  let base = moveBase ++ name
  getResponse base

-- | Get the names of moves learned by a pokemon
getMoveNames :: String -> IO (Maybe MoveNames)
getMoveNames pokemon = do
  let base = pokemonBase ++ pokemon
  getResponse base

-- | Fetch an API request from the api
getResponse :: FromJSON a => String -> IO (Maybe a)
getResponse base = do
  manager <- newManager settings
  response <- httpLbs (parseRequest_ base) manager
  if checkFound response
    then return $ decode (responseBody response)
    else return Nothing

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

allBases :: (String, String, String, String)
allBases = (pokemonBase, abilityBase, moveBase, itemBase)

-- | When running a dt command we don't know what the user has given us, so we try all bases
tryAllBases :: String -> (String, String, String, String)
tryAllBases name =
  let name' = (intercalate "-" . words) name
   in map4 (++ name') allBases

-- | Map over a 4 tuple
map4 :: (t -> d) -> (t, t, t, t) -> (d, d, d, d)
map4 f (a, b, c, d) = (f a, f b, f c, f d)

-- | Monadic map over 4 tuple, will sequentially call the function on each element.
mapM4 :: (a -> IO b) -> (a, a, a, a) -> IO (b, b, b, b)
mapM4 f (x1, x2, x3, x4) = do
  y1 <- f x1
  y2 <- f x2
  y3 <- f x3
  y4 <- f x4
  return (y1, y2, y3, y4)

fromBS4 :: (ByteString, ByteString, ByteString, ByteString) -> (Maybe Pokemon, Maybe Ability, Maybe Move, Maybe Item)
fromBS4 (x1, x2, x3, x4) = (decode x1, decode x2, decode x3, decode x4)

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
        abilities = (map _name . filter (not . isHidden)) allAbilities
        hiddenAbility = (map _name . filter isHidden) allAbilities
        ha = if length hiddenAbility == 1 then Just $ head hiddenAbility else Nothing
    return $ Pokemon name typing abilities ha baseStats Nothing (div weight 10)

data EffectEntry = EffectEntry
  { language :: String,
    eDescription :: String
  }

data Effect = Effect (Maybe Int) (Maybe String)

instance FromJSON EffectEntry where
  parseJSON (Object jsn) = EffectEntry <$> ((jsn .: "language") >>= (.: "name")) <*> (jsn .: "effect")

instance FromJSON Ability where
  parseJSON (Object jsn) = do
    name <- jsn .: "name"
    effects <- jsn .: "effect_entries"
    let effects' = filter (\(EffectEntry lang _) -> lang == "en") effects
        effect' = if (not . null) effects then (Just . eDescription . head) effects' else Nothing
    return $ Ability name effect'

instance FromJSON Item where
  parseJSON (Object jsn) = do
    name <- jsn .: "name"
    effects <- jsn .: "effect_entries"
    let effects' = filter (\(EffectEntry lang _) -> lang == "en") effects
        effect' = if (not . null) effects then (Just . eDescription . head) effects' else Nothing
    return $ Item name effect'

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
    return $ Move name mType dClass power acc (getCompleteDescription effect'')

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

getCompleteDescription :: Effect -> Maybe String
getCompleteDescription (Effect _ Nothing) = Nothing
getCompleteDescription (Effect Nothing (Just desc)) = Just desc
getCompleteDescription (Effect (Just x) (Just desc)) = Just newDesc
  where
    valueText = T.pack $ show x
    tDesc = T.pack desc
    newDesc = T.unpack $ T.replace "$effect_chance" valueText tDesc
