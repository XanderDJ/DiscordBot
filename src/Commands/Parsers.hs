{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
module Commands.Parsers where

import Commands.Auction (Auction (A), AuctionID, Participant (P), User (U))
import Commands.Types (Options)
import Commands.Utility
import Data.Char (isPunctuation, toLower)
import Data.Functor
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe
import Data.StatMultiplier
import Data.Text (Text, dropWhileEnd, pack, replace, unpack)
import qualified Data.Text as T
import Pokemon.Functions (getStat, parseNatureEffect)
import Pokemon.Types (BaseStat (BaseStat), Level, NatureEffect (NPositive))
import PokemonDB.Types (PokemonQuery (..))
import SplitSubstr (splitOnSubstrs)
import Text.Parsec
import Text.Parsec.Number (fractional, int)
import Text.Parsec.Text (Parser)
import Text.Read (readMaybe)
import Prelude hiding (words)

parseSep :: Parser Text
parseSep = T.singleton <$> char ','

parseMaybeInt :: Parser (Maybe Int)
parseMaybeInt = do
  spaces
  intString <- many digit
  if null intString
    then return Nothing
    else do
      (try (char 'k') $> Just (read intString * 1000)) <|> return (Just (read intString))

parseMaybeIntScientific :: Parser (Maybe Int)
parseMaybeIntScientific = do
  spaces
  fr <- fractional
  return $ Just (round (1000 * fr))

parseIntCommand :: String -> Parser (Maybe Int)
parseIntCommand s = string s *> spaces *> (try parseMaybeIntScientific <|> parseMaybeInt)

parseDouble :: Parser Double
parseDouble = try parseDoubleFraction <|> parseDoubleDecimal

parseDoubleFraction :: Parser Double
parseDoubleFraction = do
  nominator <- int
  char '/'
  denominator <- int
  return $ fromIntegral nominator / fromIntegral denominator

parseDoubleDecimal :: Parser Double
parseDoubleDecimal = fractional

parseUser :: Parser [User]
parseUser = do
  name <- many (noneOf "#")
  discrim <- char '#' *> many digit
  return [U (pack name) ((Just . pack) discrim)]

parseCommand :: Parser Text
parseCommand = pack <$> (char 'l' *> many letter)

parseId :: Parser Text
parseId = toId . pack <$> (spaces *> many (noneOf ","))

words :: [String] -> Parser String
words = foldr ((<|>) . try . string) (unexpected "Given prefixes not found")

keyParser :: Parser Text
keyParser = T.pack <$> many digit

-- Message example = !auction lpl 5000

registerAuctionP :: AuctionID -> User -> Parser Auction
registerAuctionP aId auctioneer = A aId . pack <$> (string "lhostauction" *> spaces *> many alphaNum) <*> parseMaybeInt <*> parseMaybeInt <*> parseMaybeInt <*> parseMaybeInt <*> pure Nothing <*> pure auctioneer <*> pure [] <*> pure []

-- example: rp mogo#5432 80000
registerParticipantP :: Parser Participant
registerParticipantP = P <$> (string "lrp" *> spaces *> parseUser) <*> parseMaybeInt <*> pure []

-- example: nom jeroni9999
nominatePlayerP :: Parser Text
nominatePlayerP = pack <$> (string "lnom" *> spaces *> many alphaNum)

-- example: b (5000|5.0k|5.0|5k)
bidP :: Parser (Maybe Int)
bidP = parseIntCommand "lb"

-- example: info lonewulfx3#3333
infoP :: Parser User
infoP = U . pack <$> (string "linfouser" *> spaces *> many (noneOf "#")) <*> (Just . pack <$> (char '#' *> many digit))

undoP :: Parser User
undoP = U . pack <$> (string "lundo" *> spaces *> many (noneOf "#")) <*> (Just . pack <$> (char '#' *> many digit))

dtP :: Parser Text
dtP = pack <$> (string "ldt " *> spaces *> many anyChar)

data NOrP = N Text | PN Text deriving (Show)

parseNOrP :: Parser NOrP
parseNOrP = (N <$> try parseN) <|> (PN <$> parsePN)

parseN :: Parser Text
parseN = toId . pack . dropHeadPattern "lss" . filter (/= ' ') <$> (many1 (noneOf ":") <* char ':')

parsePN :: Parser Text
parsePN = parseId

dropHeadPattern :: String -> String -> String
dropHeadPattern p s
  | length p > length s = s
  | p `isPrefixOf` s = drop (length p) s
  | otherwise = s

parseOutspeedLevel :: Parser (Maybe Int, Maybe Int)
parseOutspeedLevel = do
  string "los"
  spaces
  n <- try parseMaybeIntScientific <|> parseMaybeInt
  spaces
  l <- try parseMaybeIntScientific <|> parseMaybeInt
  return (n, l)

data CalcStat = CS BaseStat NatureEffect

-- "l(cs|calcstat) ((hp|atk|def|spatk|spdef|spd):basestat) (positive|neutral|negative|pos|neu|neg) [--level lvl, 100] [--iv iv, 252>] [--ev ev, 252] [--boost -1 0 +1, 0]"
parseCalcStat :: Parser CalcStat
parseCalcStat = do
  char 'l'
  try (string "cs") <|> string "calcstat"
  spaces
  stat <- string "hp" <|> string "atk" <|> string "def" <|> try (string "spatk") <|> try (string "spdef") <|> string "spd"
  spaces
  char ':'
  spaces
  bs <- read <$> many digit
  spaces
  natureEffect <- try (string "positive") <|> string "pos" <|> try (string "neutral") <|> try (string "neu") <|> try (string "negative") <|> string "neg"
  return $ CS (BaseStat (getStat stat) bs) (parseNatureEffect natureEffect)

parseMaxCalcStat :: Parser CalcStat
parseMaxCalcStat = do
  char 'l'
  try (string "ms") <|> string "maxstat"
  spaces
  stat <- string "hp" <|> string "atk" <|> string "def" <|> try (string "spa") <|> try (string "spd") <|> string "spe"
  spaces
  char ':'
  spaces
  bs <- read <$> many digit
  return $ CS (BaseStat (getStat stat) bs) NPositive

parseOptions :: Text -> (Text, M.Map Text Text)
parseOptions t =
  let ss = splitOnSubstrs ["--", "???"] (T.unpack t)
      ts = map T.pack ss
      getOpts :: M.Map Text Text -> [Text] -> M.Map Text Text
      getOpts m [] = m
      getOpts m (t : ts) = let ws = T.words t in if (not . null) ws then getOpts (M.insert (head ws) (T.intercalate " " (tail ws)) m) ts else getOpts m ts
      options = if length ts > 1 then getOpts M.empty (tail ts) else M.empty
   in ((T.strip . head) ts, options)

parseOptionsOnSep :: Char -> Text -> [(Text, M.Map Text Text)]
parseOptionsOnSep sep t = map parseOptions (T.split (== sep) t)

combineOptionsWith :: ((Text, Options) -> (Text, Options) -> (Text, Options)) -> [(Text, Options)] -> (Text, Options)
combineOptionsWith = foldl1

combineWithSep :: Semigroup b => Text -> (Text, b) -> (Text, b) -> (Text, b)
combineWithSep sep (t, opts) (t', opts') = (T.intercalate sep [t, t'], opts <> opts')

-- | Parse a command to get a list of pokemon names
-- Ex: l(bu|beatup) mon with space, mon2, mon3, ...
parseBeatUp :: Parser [Text]
parseBeatUp = (try (string "lbeatup") <|> string "lbu") *> spaces *> sepByComma parseId

sepByComma :: Parser a -> Parser [a]
sepByComma p = sepBy p parseSep

-- lct (0.9|9/10) (0.85|85/100)
parseCT :: Parser (Double, Double)
parseCT = do
  try (string "lct") <|> string "lcalctries"
  spaces
  ps <- parseDouble -- chance p of success
  spaces
  dsr <- parseDouble -- desired success rate
  return (ps, dsr)

parseCC :: Parser (Double, Int)
parseCC = do
  try (string "lcc") <|> string "lcalcchance"
  spaces
  ps <- parseDouble -- chance p of success
  spaces
  tries <- int -- desired success rate
  return (ps, tries)

parseDN :: Parser Text
parseDN = pack <$> (string "ldn" *> spaces *> many alphaNum)

parseLC :: Parser (Text, [Text])
parseLC = do
  try (string "llearn") <|> string "ll"
  spaces
  mon <- parseId
  parseSep
  moves <- sepByComma parseId
  return (mon, moves)

typeQueryParser :: Parser PokemonQuery
typeQueryParser = do
  words ["lquery", "lq"]
  spaces
  words ["coverage", "cov", "type", "types"]
  spaces
  parseSep
  spaces
  pokemon <- parseId
  parseSep
  spaces
  AllMovesFromType pokemon <$> parseId

categoryQueryParser :: Parser PokemonQuery
categoryQueryParser = do
  words ["lquery", "lq"]
  spaces
  words ["category", "cat"]
  spaces
  parseSep
  pokemon <- parseId
  parseSep
  spaces
  AllMovesFromCategory pokemon <$> parseId

categoryTypeQueryParser :: Parser PokemonQuery
categoryTypeQueryParser = do
  words ["lquery", "lq"]
  spaces
  words ["categorytype", "categorycoverage", "catcov", "covcat", "coveragecategory", "typecategory"]
  spaces
  parseSep
  pokemon <- parseId
  parseSep
  AllMovesFromCategoryAndType pokemon <$> parseId <*> (parseSep *> parseId)

singleParameterQuery :: [String] -> (T.Text -> PokemonQuery) -> Parser PokemonQuery
singleParameterQuery queryType queryConstructor = do
  words ["lquery", "lq"]
  spaces
  words queryType
  spaces
  parseSep
  queryConstructor <$> parseId

allClericMovesParser :: Parser PokemonQuery
allClericMovesParser = singleParameterQuery ["cleric"] AllClericMoves

hazardMovesQueryParser :: Parser PokemonQuery
hazardMovesQueryParser = singleParameterQuery ["hazard"] AllHazardMoves

hazardControlParser :: Parser PokemonQuery
hazardControlParser = singleParameterQuery ["hazardcontrol"] AllHazardControl

allMovesParser :: Parser PokemonQuery
allMovesParser = singleParameterQuery ["all moves"] AllMoves

allPokemonsWithAbilityParser :: Parser PokemonQuery
allPokemonsWithAbilityParser = singleParameterQuery ["ability"] AllPokemonsWithAbility

allPokemonsWithMoveParser :: Parser PokemonQuery
allPokemonsWithMoveParser = singleParameterQuery ["move"] AllPokemonsWithMove

allPriority :: Parser PokemonQuery
allPriority = singleParameterQuery ["priority", "prio"] AllPriorityMoves

allRecoveryParser :: Parser PokemonQuery
allRecoveryParser = singleParameterQuery ["recovery", "heal", "healing"] AllRecoveryMoves

allBoostParser :: Parser PokemonQuery
allBoostParser = singleParameterQuery ["boost", "boosting", "setup"] AllSetUpMoves

allScreensParser :: Parser PokemonQuery
allScreensParser = singleParameterQuery ["screens", "screen"] AllScreens

allStatusParser :: Parser PokemonQuery
allStatusParser = singleParameterQuery ["status"] AllStatusMoves

allPokemonsWithStatParser :: Parser PokemonQuery
allPokemonsWithStatParser = do
  words ["lquery", "lq"]
  spaces
  words ["stat"]
  spaces
  parseSep
  stat <- parseId
  spaces
  parseSep
  AllPokemonWithStat stat . read . (\s -> if null s then "0" else s) <$> many digit

allPokemonsWithMoveFromTeamParser :: Parser PokemonQuery
allPokemonsWithMoveFromTeamParser = do
  words ["lquery", "lq"]
  spaces
  words ["moveteam", "movet"]
  spaces
  parseSep
  move <- parseId
  spaces
  parseSep
  AllPokemonsWithMoveFromTeam move <$> sepByComma parseId

queryParser :: Parser PokemonQuery
queryParser =
  DT <$> dtP
    <||> toLearnQuery <$> parseLC
    <||> typeQueryParser
    <||> categoryQueryParser
    <||> categoryTypeQueryParser
    <||> allClericMovesParser
    <||> allScreensParser
    <||> allStatusParser
    <||> hazardMovesQueryParser
    <||> hazardControlParser
    <||> allBoostParser
    <||> allRecoveryParser
    <||> allPriority
    <||> allPokemonsWithAbilityParser
    <||> allPokemonsWithMoveParser
    <||> allMovesParser
    <||> allPokemonsWithStatParser
    <||> allPokemonsWithMoveFromTeamParser
  where
    toLearnQuery (p, ms) = Learn p ms

(<||>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
(<||>) a b = try a <|> b

infixl 3 <||>