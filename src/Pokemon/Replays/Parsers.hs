module Pokemon.Replays.Parsers where

import Data.Functor (($>))
import qualified Data.Text as T
import Pokemon.Replays.Types
import qualified Pokemon.Types as T
import Text.Parsec
import Text.Parsec.Text (Parser)

-- UTILITY PARSERS

parseSep1 :: Parser [Char]
parseSep1 = many1 $ char '|'

parseSepJ1 :: Parser Char
parseSepJ1 = char '|'

parseSep0 :: Parser [Char]
parseSep0 = many $ char '|'

parseMNumber :: Parser (Maybe Int)
parseMNumber = do
  numberStr <- many digit
  if null numberStr
    then pure Nothing
    else return $ Just (read numberStr)

parseNumber :: Parser Int
parseNumber = read <$> many digit

parseTillSep :: Parser T.Text
parseTillSep = T.pack <$> many (noneOf "|")

parseMaybeTillSep :: Parser (Maybe T.Text)
parseMaybeTillSep = (\x -> if T.null x then Nothing else Just x) <$> parseTillSep

parseDetails :: Parser T.Text
parseDetails = T.pack . head <$> many (noneOf ",|") `sepBy` char ','

parsePosition :: Parser Position
parsePosition = char 'p' *> (parseP1 <|> parseP2 <|> parseP3 <|> parseP4)

parseP1 :: Parser Position
parseP1 = char '1' *> (oneOf "abc" <|> lookAhead (oneOf "|:")) $> P1

parseP2 :: Parser Position
parseP2 = char '2' *> (oneOf "abc" <|> lookAhead (oneOf "|:")) $> P2

parseP3 :: Parser Position
parseP3 = char '3' *> (oneOf "abc" <|> lookAhead (oneOf "|:")) $> P3

parseP4 :: Parser Position
parseP4 = char '4' *> (oneOf "abc" <|> lookAhead (oneOf "|:")) $> P4

parseTag :: Parser Tag
parseTag =
  char '['
    *> ( parseFrom
           <|> parseConsumed
           <|> parseOf
           <|> parseUK
           <|> parseNT
           <|> parseDmg
           <|> parseEat
           <|> parseAnimTag
           <|> parseMissTag
           <||> parseMSG
           <|> parsePartiallyTrapped
           <|> parseStill
           <||> parseSilent
           <|> parseWisher
           <||> parseWeaken
           <|> parseIdentify
       )

parseConsumed :: Parser Tag
parseConsumed = "consumed]" $>> CONSUMED

parsePartiallyTrapped :: Parser Tag
parsePartiallyTrapped = "partiallytrapped]" $>> PARTIALLYTRAPPED

parseDmg :: Parser Tag
parseDmg = "damage]" $>> DAMAGE

parseFrom :: Parser Tag
parseFrom = string "from]" *> spaces *> (string "item:" <|> string "ability:" <|> string "move:" <|> lookAhead (many (noneOf "|"))) *> spaces *> (From <$> undefined)

parseFromItem :: Parser FromTag
parseFromItem = string "item:" *> spaces *> (ITEM <$> parseTillSep)

parseFromAbility :: Parser FromTag
parseFromAbility = string "ability:" *> spaces *> (ABILITY <$> parseTillSep)

parseFromMove :: Parser FromTag
parseFromMove = string "move:" *> spaces *> (MOVE <$> parseTillSep)

parseFromRecoil :: Parser FromTag
parseFromRecoil = spaces *> string "Recoil" $> RECOIL

parseFromStatus :: Parser FromTag
parseFromStatus = spaces *> lookAhead (string "psn" <||> string "brn" <|> string "frz" <|> string "slp" <|> string "par" <|> string "tox") *> (STATUS . read . T.unpack <$> parseTillSep)

parseFromWeather :: Parser FromTag
parseFromWeather = spaces *> ( WEATHER <$> (parseSandstorm <||> parseSunnyDay <|> parseRainDance <|> parseHail))

parseHail :: Parser T.Weather
parseHail = string "Hail" $> T.HAIL

parseSandstorm :: Parser T.Weather
parseSandstorm = string "Sandstorm" $> T.SANDSTORM

parseSunnyDay :: Parser T.Weather
parseSunnyDay = string "SunnyDay" $> T.SUN

parseRainDance :: Parser T.Weather
parseRainDance = string "RainDance" $> T.RAIN

parseOf :: Parser Tag
parseOf = do
  string "of]"
  spaces
  parsePosition
  char ':'
  spaces
  Of <$> parseNick

parseMissTag :: Parser Tag
parseMissTag = "miss]" $>> MISS

parseStill :: Parser Tag
parseStill = "still]" $>> STILL

parseSilent :: Parser Tag
parseSilent = "silent]" $>> SILENT

parseUK :: Parser Tag
parseUK = "upkeep]" $>> UK

parseNT :: Parser Tag
parseNT = "notarget]" $>> NOTARGET

parseEat :: Parser Tag
parseEat = "eat]" $>> EAT

parseAnimTag :: Parser Tag
parseAnimTag = string "anim]" *> spaces *> (ANIM <$> parseTillSep)

parseMSG :: Parser Tag
parseMSG = "msg]" $>> MSG

parseWisher :: Parser Tag
parseWisher = "wisher]" *>> spaces *> (WISHER <$> parseTillSep)

parseIdentify :: Parser Tag
parseIdentify = "identify]" $>> IDENTIFY

parseWeaken :: Parser Tag
parseWeaken = "weaken]" $>> WEAKEN

parseTags :: Parser [Tag]
parseTags = parseTag `sepBy` char '|'

parseSwitchIn :: (Nick -> T.Text -> Health -> [Tag] -> a) -> String -> Parser a
parseSwitchIn dataConstructor pre = pre *>> dataConstructor <$> parseNick <*|> parseDetails <*|> parseHp <*|*> parseTags

parseNick :: Parser Nick
parseNick = do
  pos <- parsePosition
  char ':' *> spaces
  nick <- parseTillSep
  return (pos, nick)

parseMaybeNick :: Parser (Maybe Nick)
parseMaybeNick = do
  txt <- lookAhead $ many $ char 'p'
  if null txt then pure Nothing else Just <$> parseNick

parseHp :: Parser Health
parseHp = parseHpNonFaint <||> parseHpFaint

parseHpNonFaint :: Parser Health
parseHpNonFaint = do
  currentHp <- read <$> many digit
  char '/'
  totalHp <- read <$> many digit
  spaces
  status <- parseTillSep
  return (currentHp, totalHp, status)

parseHpFaint :: Parser Health
parseHpFaint = do
  currentHp <- read <$> many digit
  spaces
  status <- parseTillSep
  return (currentHp, 0, status)

parseChange :: (Nick -> T.Text -> [Tag] -> a) -> String -> Parser a
parseChange dataConstructor pre = pre *>> dataConstructor <$> parseNick <*|> parseDetails <*|*> parseTags

-- | Parse with just 1 separator in between
(<*|>) :: Parser (a -> b) -> Parser a -> Parser b
(<*|>) a b = a <* parseSepJ1 <*> b

-- | Parse with 0 or more separators in between
(<*|*>) :: Parser (a -> b) -> Parser a -> Parser b
(<*|*>) a b = a <* parseSep0 <*> b

-- | Parse with 1 or more separators in between
(<*|+>) :: Parser (a -> b) -> Parser a -> Parser b
(<*|+>) a b = a <* parseSep1 <*> b

-- | Run parser a after string pre is succesfully parsed, function is usefull for eliminating brackets and duplicate code
(*>>) :: String -> Parser a -> Parser a
(*>>) pre a = string pre *> a

-- | Wrap a in parser context after prefix is succesfully parsed. Function is useful for eliminating duplicate code
($>>) :: String -> a -> Parser a
($>>) pre a = string pre $> a

infixl 4 <*|>, <*|*>, <*|+>

infixl 3 *>>, $>>

parseBoostType :: (Nick -> T.Text -> Int -> a) -> String -> Parser a
parseBoostType constructor pre = string pre *> (constructor <$> parseNick <*|> parseTillSep <*|> parseNumber)

parseDash :: Parser [Char]
parseDash = do
  nextDashes <- lookAhead (many (oneOf "-"))
  if null nextDashes then pure "-" else string nextDashes

-- PARSERS OF MESSAGES

parseAbility' :: Parser Ability
parseAbility' = "ability|" *>> Ability <$> parseNick <*|> parseTillSep <*|*> parseTags

parseAbility = RMA <$> parseAbility'

parseAnim' :: Parser Anim
parseAnim' = "anim|" *>> Anim <$> parseNick <*|> parseTillSep <*|> parseNick

parseAnim = RMAnim <$> parseAnim'

parseActivate' :: Parser Activate
parseActivate' = "activate|" *>> Activate <$> parseNick <*|> parseTillSep <*|*> parseTags

parseActivate = RMActivate <$> parseActivate'

parseBlock' :: Parser Block
parseBlock' = "block|" *>> Block <$> parseNick <*|> parseTillSep <*|> parseTillSep <*|> parseTillSep <*|*> parseTags

parseBlock :: Parser ReplayMessage
parseBlock = RMB <$> parseBlock'

parseBoost' :: Parser Boost
parseBoost' = parseBoostType Boost "boost|"

parseBoost = RMBoost <$> parseBoost'

parseBurst' :: Parser Burst
parseBurst' = "burst|" *>> Burst <$> parseNick <*|> parseDetails <*|> parseTillSep

parseBurst = RMBurst <$> parseBurst'

parseCant' :: Parser Cant
parseCant' = do
  string "cant|"
  nick <- parseNick
  parseSepJ1
  reason <- parseTillSep
  seps <- parseSep0
  if null seps
    then do
      return $ Cant nick reason Nothing
    else do
      Cant nick reason . Just <$> parseTillSep

parseCant :: Parser ReplayMessage
parseCant = RMCant <$> parseCant'

parseCenter' :: Parser Center
parseCenter' = "center" $>> CENTER

parseCenter = RMCenter <$> parseCenter'

parseChat' :: Parser Chat
parseChat' = string "c|" *> spaces *> (Chat <$> parseTillSep <*|> parseTillSep)

parseChat :: Parser ReplayMessage
parseChat = RMChat <$> parseChat'

parseClearAllBoosts' :: Parser ClearAllBoosts
parseClearAllBoosts' = "clearallboost" $>> CLEARALLBOOSTS

parseClearAllBoosts = RMCAB <$> parseClearAllBoosts'

parseClearBoost' :: Parser ClearBoost
parseClearBoost' = "clearboost|" *>> ClearBoost <$> parseNick

parseClearBoost = RMCB <$> parseClearBoost'

parseClearNegativeBoost' :: Parser ClearNegativeBoost
parseClearNegativeBoost' = "clearnegativeboost|" *>> ClearNegativeBoost <$> parseNick

parseClearNegativeBoost = RMCNB <$> parseClearNegativeBoost'

parseClearPoke' :: Parser ClearPokemon
parseClearPoke' = "clearpoke" $>> ClearPoke

parseClearPoke :: Parser ReplayMessage
parseClearPoke = RMCP <$> parseClearPoke'

parseClearPositiveBoost' :: Parser ClearPositiveBoost
parseClearPositiveBoost' = "clearpositiveboost|" *>> ClearPositiveBoost <$> parseNick <*|> parseNick <*|> parseTillSep

parseClearPositiveBoost = RMCPB <$> parseClearPositiveBoost'

parseCombine' :: Parser Combine
parseCombine' = "combine" $>> COMBINE

parseCombine = RMCombine <$> parseCombine'

parseCopyBoost' :: Parser CopyBoost
parseCopyBoost' = "copyboost|" *>> CopyBoost <$> parseNick <*|> parseNick <*|*> parseTags

parseCopyBoost :: Parser ReplayMessage
parseCopyBoost = RMCopyBoost <$> parseCopyBoost'

parseCrit' :: Parser Crit
parseCrit' = "crit|" *>> Crit <$> parseNick

parseCrit = RMC <$> parseCrit'

parseCureStatus' :: Parser CureStatus
parseCureStatus' = "curestatus|" *>> CureStatus <$> parseNick <*|> parseTillSep

parseCureStatus = RMCS <$> parseCureStatus'

parseCureTeam' :: Parser CureTeam
parseCureTeam' = "cureteam|" *>> CureTeam <$> parseNick

parseCureTeam = RMCT <$> parseCureTeam'

parseDamage' :: Parser Damage
parseDamage' = "damage|" *>> Damage <$> parseNick <*|> parseHp <*|*> parseTags

parseDamage :: Parser ReplayMessage
parseDamage = RMDmg <$> parseDamage'

parseDetailsChange' :: Parser DetailsChange
parseDetailsChange' = parseChange DetailsChange "detailschange|"

parseDetailsChange :: Parser ReplayMessage
parseDetailsChange = RMDC <$> parseDetailsChange'

parseDrag' :: Parser Drag
parseDrag' = parseSwitchIn Drag "drag|"

parseDrag :: Parser ReplayMessage
parseDrag = RMDrag <$> parseDrag'

parseEndAbility' :: Parser EndAbility
parseEndAbility' = "endability|" *>> EndAbility <$> parseNick <*|*> parseTags

parseEndAbility = RMEA <$> parseEndAbility'

parseEndItem' :: Parser EndItem
parseEndItem' = "enditem|" *>> EndItem <$> parseNick <*|> parseTillSep <*|*> parseTags

parseEndItem = RMEI <$> parseEndItem'

parseFail' :: Parser Failed
parseFail' = do
  string "fail|"
  nick <- parseNick
  parseSep0
  move <- parseTillSep
  if T.null move then return $ Failed nick Nothing else return $ Failed nick (Just move)

parseFail :: Parser ReplayMessage
parseFail = RMFailed <$> parseFail'

parseFaint' :: Parser Faint
parseFaint' = "faint|" *>> Faint <$> parseNick

parseFaint :: Parser ReplayMessage
parseFaint = RMF <$> parseFaint'

parseFieldStart' :: Parser FieldStart
parseFieldStart' = "fieldstart|" *>> FieldStart <$> parseTillSep <*|*> parseTags

parseFieldStart = RMFS <$> parseFieldStart'

parseFieldActivate' :: Parser FieldActivate
parseFieldActivate' = "fieldactivate|" *>> FieldActivate <$> parseTillSep

parseFieldActivate = RMFA <$> parseFieldActivate'

parseFieldEnd' :: Parser FieldEnd
parseFieldEnd' = "fieldend|" *>> FieldEnd <$> parseTillSep <*|*> parseTags

parseFieldEnd = RMFE <$> parseFieldEnd'

parseFormChange' :: Parser FormeChange
parseFormChange' = parseChange FormeChange "formechange|"

parseFormChange :: Parser ReplayMessage
parseFormChange = RMFC <$> parseFormChange'

parseGameType' :: Parser GameType
parseGameType' = "gametype|" *>> parseGT'

parseSingles :: Parser GameType
parseSingles = "singles" $>> SINGLES

parseDoubles :: Parser GameType
parseDoubles = "doubles" $>> DOUBLES

parseTriples :: Parser GameType
parseTriples = "triples" $>> TRIPLES

parseMulti :: Parser GameType
parseMulti = "multi" $>> MULTI

parseFFA :: Parser GameType
parseFFA = "free-for-all" $>> FFA

parseGT' :: Parser GameType
parseGT' = parseSingles <|> parseDoubles <|> parseMulti <|> parseFFA

parseGameType :: Parser ReplayMessage
parseGameType = RMGT <$> parseGameType'

parseGen' :: Parser Gen
parseGen' = "gen|" *>> Gen . read <$> many digit

parseGen :: Parser ReplayMessage
parseGen = RMG <$> parseGen'

parseHeal' :: Parser Heal
parseHeal' = "heal|" *>> Heal <$> parseNick <*|> parseHp <*|*> parseTags

parseHeal :: Parser ReplayMessage
parseHeal = RMH <$> parseHeal'

parseHint' :: Parser Hint
parseHint' = "hint|" *>> Hint <$> parseTillSep

parseHint = RMHint <$> parseHint'

parseHitcount' :: Parser HitCount
parseHitcount' = "hitcount|" *>> HitCount <$> parseNick <*|> parseNumber

parseHitcount = RMHC <$> parseHitcount'

parseHTML' :: Parser HTML
parseHTML' = "html|" *>> HTML <$> parseTillSep

parseHTML :: Parser ReplayMessage
parseHTML = RMHTML <$> parseHTML'

parseImmune' :: Parser Immune
parseImmune' = "immune|" *>> Immune <$> parseNick

parseImmune = RMImmune <$> parseImmune'

parseInactiveOff' :: Parser InactiveOff
parseInactiveOff' = "inactiveoff|" *>> InactiveOff <$> parseTillSep

parseInactiveOff :: Parser ReplayMessage
parseInactiveOff = RMInactiveOff <$> parseInactiveOff'

parseInactive' :: Parser Inactive
parseInactive' = "inactive|" *>> Inactive <$> parseTillSep

parseInactive :: Parser ReplayMessage
parseInactive = RMInactive <$> parseInactive'

parseInvertBoost' :: Parser InvertBoost
parseInvertBoost' = "invertboost|" *>> InvertBoost <$> parseNick <*|*> parseTags

parseInvertBoost = RMInvertBoost <$> parseInvertBoost'

parseItem' :: Parser Item
parseItem' = "item|" *>> Item <$> parseNick <*|> parseTillSep <*|*> parseTags

parseItem = RMI <$> parseItem'

-- | Parse Join type messages "|j|"
parseJoin' :: Parser Join
parseJoin' = "j|" *>> spaces *> (Join <$> parseTillSep)

parseJoin :: Parser ReplayMessage
parseJoin = RMJ <$> parseJoin'

parseLeave' :: Parser Leave
parseLeave' = "l|" *>> spaces *> (Leave <$> parseTillSep)

parseLeave :: Parser ReplayMessage
parseLeave = RML <$> parseLeave'

parseMega' :: Parser Mega
parseMega' = "mega|" *>> Mega <$> parseNick <*|> parseTillSep

parseMega = RMMega <$> parseMega'

parseMessage' :: Parser Message
parseMessage' = "message|" *>> Message <$> parseTillSep

parseMessage = RMMessage <$> parseMessage'

parseMiss' :: Parser Miss
parseMiss' = do
  string "miss|"
  nick <- parseNick
  parseSep0
  target <- parseTillSep
  if T.null target then return $ Miss nick Nothing else return $ Miss nick (Just target)

parseMiss :: Parser ReplayMessage
parseMiss = RMMiss <$> parseMiss'

parseMove' :: Parser Move
parseMove' = do
  string "move|"
  nick <- parseNick
  parseSepJ1
  move <- parseTillSep
  parseSepJ1
  lookahead <- lookAhead parseTillSep
  if T.null lookahead
    then do
      parseSep0
      Move nick move Nothing <$> parseTags
    else do
      target <- parseNick
      parseSep0
      Move nick move (Just target) <$> parseTags

parseMove :: Parser ReplayMessage
parseMove = RMMove <$> parseMove'

parseMustRecharge' :: Parser MustRecharge
parseMustRecharge' = "mustrecharge|" *>> MustRecharge <$> parseTillSep

parseMustRecharge = RMMustRecharge <$> parseMustRecharge'

parseName' :: Parser Name
parseName' = "n|" *>> spaces *> (N <$> parseTillSep)

parseName = RMName <$> parseName'

parseNoTarget' :: Parser NoTarget
parseNoTarget' = "notarget|" *>> NoTarget <$> parseTillSep

parseNoTarget :: Parser ReplayMessage
parseNoTarget = RMNT <$> parseNoTarget'

parseNothing' :: Parser Nothing
parseNothing' = "nothing" $>> NOTHING

parseNothing = RMNothing <$> parseNothing'

parsePlayer' :: Parser Player
parsePlayer' = "player|" *>> Pl <$> parsePosition <*|> parseMaybeTillSep <*|*> parseMaybeTillSep <*|*> parseMNumber

parsePlayer :: Parser ReplayMessage
parsePlayer = RMPL <$> parsePlayer'

parsePoke' :: Parser Poke
parsePoke' = do
  string "poke|"
  pos <- parsePosition
  parseSepJ1
  name <- parseDetails
  parseSepJ1
  item <- many (noneOf "|")
  if not $ null item
    then pure $ Poke pos name (Just . T.pack $ item)
    else pure $ Poke pos name Nothing

parsePoke :: Parser ReplayMessage
parsePoke = RMPoke <$> parsePoke'

parsePrepare' :: Parser Prepare
parsePrepare' = "prepare|" *>> Prepare <$> parseNick <*|> parseTillSep <*|*> parseMaybeNick

parsePrepare = RMPrepare <$> parsePrepare'

parsePrimal' :: Parser Primal
parsePrimal' = "primal|" *>> Primal <$> parseNick

parsePrimal = RMPrimal <$> parsePrimal'

parseRated' :: Parser Rated
parseRated' = do
  string "rated"
  sep <- many (oneOf "|")
  if null sep
    then pure (RATED Nothing)
    else do
      RATED . Just <$> parseTillSep

parseRated :: Parser ReplayMessage
parseRated = RMR <$> parseRated'

parseRaw' :: Parser Raw
parseRaw' = "raw|" *>> Raw <$> parseTillSep

parseRaw = RMRaw <$> parseRaw'

parseReplace' :: Parser Replace
parseReplace' = "replace|" *>> Replace <$> parseNick <*|> parseDetails <*|*> parseTags

parseReplace :: Parser ReplayMessage
parseReplace = RMReplace <$> parseReplace'

parseResisted' :: Parser Resisted
parseResisted' = "resisted|" *>> Resisted <$> parseNick

parseResisted = RMResist <$> parseResisted'

parseRule' :: Parser Rule
parseRule' = "rule|" *>> Rule <$> parseTillSep

parseRule :: Parser ReplayMessage
parseRule = RMRule <$> parseRule'

parseSetBoost' :: Parser SetBoost
parseSetBoost' = parseBoostType SetBoost "setboost|"

parseSetBoost = RMSetBoost <$> parseSetBoost'

parseSetHp' :: Parser SetHP
parseSetHp' = "sethp|" *>> SetHP <$> parseNick <*|> parseHp <*|*> parseTags

parseSetHp :: Parser ReplayMessage
parseSetHp = RMSHP <$> parseSetHp'

parseSideStart' :: Parser SideStart
parseSideStart' = "sidestart|" *>> SideStart <$> parseNick <*|> parseTillSep

parseSideStart = RMSS <$> parseSideStart'

parseSideEnd' :: Parser SideEnd
parseSideEnd' = "sideend|" *>> SideEnd <$> parseNick <*|> parseTillSep

parseSideEnd = RMSE <$> parseSideEnd'

parseSingleMove' :: Parser SingleMove
parseSingleMove' = "singlemove|" *>> SingleMove <$> parseNick <*|> parseTillSep

parseSingleMove = RMSM <$> parseSingleMove'

parseSingleTurn' :: Parser SingleTurn
parseSingleTurn' = "singleturn|" *>> SingleTurn <$> parseNick <*|> parseTillSep

parseSingleTurn = RMST <$> parseSingleTurn'

parseStart' :: Parser Start
parseStart' = "start" $>> START

parseStart :: Parser ReplayMessage
parseStart = RMS <$> parseStart'

parseStatus' :: Parser StatusRM
parseStatus' = "status|" *>> Status <$> parseNick <*|> parseTillSep

parseStatus :: Parser ReplayMessage
parseStatus = RMStatus <$> parseStatus'

parseSuperEffective' :: Parser SuperEffective
parseSuperEffective' = "supereffective|" *>> SuperEffective <$> parseNick

parseSuperEffective = RMSuperEffective <$> parseSuperEffective'

parseSwap' :: Parser Swap
parseSwap' = "swap|" *>> Swap <$> parseNick <*|> parseTillSep <*|*> parseTags

parseSwap :: Parser ReplayMessage
parseSwap = RMSwap <$> parseSwap'

parseSwapBoost' :: Parser SwapBoost
parseSwapBoost' = "swapboost|" *>> SwapBoost <$> parseNick <*|> parseNick <*|> (T.pack <$> many (noneOf " ,|")) `sepBy` string ", " <*|*> parseTags

parseSwapBoost = RMSwapBoost <$> parseSwapBoost'

parseSwitch' :: Parser Switch
parseSwitch' = parseSwitchIn Switch "switch|"

parseSwitch :: Parser ReplayMessage
parseSwitch = RMSwitch <$> parseSwitch'

parseTeamPreview' :: Parser TeamPreview
parseTeamPreview' = "teampreview" $>> TeamPreview

parseTeamPreview = RMTP <$> parseTeamPreview'

parseTransform' :: Parser Transform
parseTransform' = "transform|" *>> Transform <$> parseNick <*|> parseNick <*|*> parseTags

parseTransform = RMTransform <$> parseTransform'

parseTS' :: Parser TeamSize
parseTS' = "teamsize|" *>> TS <$> parseTillSep <*|> parseNumber

parseTS :: Parser ReplayMessage
parseTS = RMTS <$> parseTS'

parseTier' :: Parser Tier
parseTier' = "tier|" *>> Tier <$> parseTillSep

parseTier :: Parser ReplayMessage
parseTier = RMT <$> parseTier'

parseTie' :: Parser Tie
parseTie' = string "tie" $> TIE <* eof

parseTie :: Parser ReplayMessage
parseTie = RMTie <$> parseTie'

parseTimeStamp' :: Parser TimeStamp
parseTimeStamp' = "t:|" *>> TimeStamp <$> parseNumber

parseTimeStamp :: Parser ReplayMessage
parseTimeStamp = RMTimeStamp <$> parseTimeStamp'

parseTurn' :: Parser Turn
parseTurn' = "turn|" *>> Turn <$> parseNumber

parseTurn :: Parser ReplayMessage
parseTurn = RMTurn <$> parseTurn'

parseUnboost' :: Parser Unboost
parseUnboost' = parseBoostType Unboost "unboost|"

parseUnboost = RMUnboost <$> parseUnboost'

parseUpkeep' :: Parser Upkeep
parseUpkeep' = "upkeep" $>> UPKEEP

parseUpkeep :: Parser ReplayMessage
parseUpkeep = RMU <$> parseUpkeep'

parseVolatileStart' :: Parser VolatileStart
parseVolatileStart' = "start|" *>> VolatileStart <$> parseNick <*|> parseTillSep

parseVolatileStart = RMVS <$> parseVolatileStart'

parseVolatileEnd' :: Parser VolatileEnd
parseVolatileEnd' = "end|" *>> VolatileEnd <$> parseNick <*|> parseTillSep

parseVolatileEnd = RMVE <$> parseVolatileEnd'

parseWaiting' :: Parser Waiting
parseWaiting' = "waiting|" *>> Waiting <$> parseNick <*|> parseNick

parseWaiting :: Parser ReplayMessage
parseWaiting = RMWaiting <$> parseWaiting'

parseWeather' :: Parser WeatherRM
parseWeather' = "weather|" *>> Weather <$> parseTillSep <*|*> parseTags

parseWeather = RMWeather <$> parseWeather'

parseWin' :: Parser Win
parseWin' = "win|" *>> Win <$> parseTillSep

parseWin :: Parser ReplayMessage
parseWin = RMW <$> parseWin'

parseZBroken' :: Parser ZBroken
parseZBroken' = "zbroken|" *>> ZBroken <$> parseNick

parseZBroken = RMZB <$> parseZBroken'

parseZMove' :: Parser ZMove
parseZMove' = "zpower|" *>> ZMove <$> parseNick <*|*> parseTags

parseZMove = RMZM <$> parseZMove'

parseDelimiter' :: Parser Delimiter
parseDelimiter' = eof $> DELIMITER

parseDelimiter :: Parser ReplayMessage
parseDelimiter = RMD <$> parseDelimiter'

parseUnsupported' :: Parser Unsupported
parseUnsupported' = Unsupported . T.pack <$> many anyChar

parseUnsupported :: Parser ReplayMessage
parseUnsupported = RMUnsupported <$> parseUnsupported'

-- BIG PARSE FUNCTION

(<||>) :: Parser a -> Parser a -> Parser a
(<||>) a b = try a <|> b

parseReplayMessage :: Parser ReplayMessage
parseReplayMessage =
  (parseSep0 *> parseDash)
    *> ( parseDelimiter
           <||> parseAbility
           <||> parseActivate
           <||> parseAnim
           <||> parseBlock
           <||> parseBoost
           <||> parseBurst
           <||> parseCant
           <||> parseCenter
           <||> parseChat
           <||> parseClearAllBoosts
           <||> parseClearBoost
           <||> parseClearNegativeBoost
           <||> parseClearPoke
           <||> parseClearPositiveBoost
           <||> parseCombine
           <||> parseCopyBoost
           <||> parseCrit
           <||> parseCureStatus
           <||> parseCureTeam
           <||> parseDamage
           <||> parseDetailsChange
           <||> parseDrag
           <||> parseEndAbility
           <||> parseEndItem
           <||> parseFail
           <||> parseFaint
           <||> parseFieldStart
           <||> parseFieldActivate
           <||> parseFieldEnd
           <||> parseFormChange
           <||> parseGameType
           <||> parseGen
           <||> parseHeal
           <||> parseHint
           <||> parseHitcount
           <||> parseHTML
           <||> parseImmune
           <||> parseInactiveOff
           <||> parseInactive
           <||> parseInvertBoost
           <||> parseItem
           <||> parseJoin
           <||> parseLeave
           <||> parseMega
           <||> parseMessage
           <||> parseMiss
           <||> parseMove
           <||> parseMustRecharge
           <||> parseName
           <||> parseNoTarget
           <||> parseNothing
           <||> parsePlayer
           <||> parsePoke
           <||> parsePrepare
           <||> parsePrimal
           <||> parseRated
           <||> parseRaw
           <||> parseReplace
           <||> parseResisted
           <||> parseRule
           <||> parseSetBoost
           <||> parseSetHp
           <||> parseSideStart
           <||> parseSideEnd
           <||> parseSingleMove
           <||> parseSingleTurn
           <||> parseStart
           <||> parseStatus
           <||> parseSuperEffective
           <||> parseSwap
           <||> parseSwapBoost
           <||> parseSwitch
           <||> parseTeamPreview
           <||> parseTransform
           <||> parseTS
           <||> parseTier
           <||> parseTie
           <||> parseTimeStamp
           <||> parseTurn
           <||> parseUnboost
           <||> parseUpkeep
           <||> parseVolatileStart
           <||> parseVolatileEnd
           <||> parseWaiting
           <||> parseWeather
           <||> parseWin
           <||> parseZBroken
           <||> parseZMove
           <||> parseUnsupported
       )
