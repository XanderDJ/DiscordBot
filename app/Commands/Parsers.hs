module Commands.Parsers where

import Commands.Auction (Auction (A), AuctionID, Participant (P), User (U))
import Data.Char (isPunctuation, toLower)
import Data.Functor
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe
import Data.StatMultiplier
import Data.Text (Text, dropWhileEnd, pack, replace, unpack)
import qualified Data.Text as T
import Pokemon.Functions (getNatureEffect, getStat)
import Pokemon.Types (BaseStat (BaseStat), Level, NatureEffect (NPositive))
import Text.Parsec
import Text.Parsec.Text (Parser)
import Text.Parsec.Number ( fractional, int ) 
import Commands.Utility (toId)

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
  base <- many digit
  char '.'
  ground <- many digit
  let fixedGround = unpack . dropWhileEnd ('0' ==) . pack $ ground
  if null fixedGround || null base
    then return Nothing
    else do
      let x = read base * 1000
          y = read fixedGround * 100
      return $ Just (x + y)

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

parseUser :: Parser User
parseUser = U . pack <$> many (noneOf "#") <*> (char '#' *> parseMaybeInt)

parseCommand :: Parser Text
parseCommand = pack <$> (char 'l' *> many letter)

parseId :: Parser Text
parseId = toId . pack  <$> (spaces *> many (noneOf ","))

-- Message example = !auction lpl 5000

registerAuctionP :: AuctionID -> User -> Parser Auction
registerAuctionP aId auctioneer = A aId . pack <$> (string "lhostauction" *> spaces *> many alphaNum) <*> parseMaybeInt <*> parseMaybeInt <*> parseMaybeInt <*> pure Nothing <*> pure auctioneer <*> pure []

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
infoP = U . pack <$> (string "linfouser" *> spaces *> many (noneOf "#")) <*> (char '#' *> parseMaybeInt)

undoP :: Parser User
undoP = U . pack <$> (string "lundo" *> spaces *> many (noneOf "#")) <*> (char '#' *> parseMaybeInt)

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
  char ':'
  bs <- read <$> many digit
  spaces
  natureEffect <- try (string "positive") <|> string "pos" <|> try (string "neutral") <|> try (string "neu") <|> try (string "negative") <|> string "neg"
  return $ CS (BaseStat (getStat stat) bs) (getNatureEffect natureEffect)

parseMaxCalcStat :: Parser CalcStat
parseMaxCalcStat = do
  char 'l'
  try (string "ms") <|> string "maxstat"
  spaces
  stat <- string "hp" <|> string "atk" <|> string "def" <|> try (string "spatk") <|> try (string "spdef") <|> string "spd"
  char ':'
  bs <- read <$> many digit
  return $ CS (BaseStat (getStat stat) bs) (NPositive)

parseOptions :: Text -> (Text, M.Map Text Text)
parseOptions t =
  let ts = T.split (\c -> c == '-' || c == '—') t
      getOpts :: M.Map Text Text -> [Text] -> M.Map Text Text
      getOpts m [] = m
      getOpts m (t : ts) = let ws = T.words t in if (not . null) ws then getOpts (M.insert (head ws) (T.intercalate " " (tail ws)) m) ts else getOpts m ts
      options = if length ts > 1 then getOpts M.empty (tail ts) else M.empty
   in ((T.strip . head) ts, options)

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