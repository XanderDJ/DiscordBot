module Commands.Parsers where

import Commands.Auction (Auction (A), AuctionID, Participant (P), User (U))
import Data.Char (toLower)
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

parseUser :: Parser User
parseUser = U . pack <$> many (noneOf "#") <*> (char '#' *> parseMaybeInt)

parseCommand :: Parser Text
parseCommand = pack <$> (char 'l' *> many letter)

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

dtP :: Parser String
dtP = map (\s -> if s == ' ' then '-' else toLower s) <$> (string "ldt " *> spaces *> many anyChar)

data NOrP = N Text | PN Text deriving (Show)

parseNOrP :: Parser NOrP
parseNOrP = (N <$> try parseN) <|> (PN <$> parsePN)

parseN :: Parser Text
parseN = pack . dropHeadPattern "lss" . filter (/= ' ') <$> (many1 (noneOf ":") <* char ':')

parsePN :: Parser Text
parsePN = pack . map (\c -> if c == ' ' then '-' else toLower c) <$> many1 anyChar

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
  let ts = T.splitOn "--" t
      getOpts :: M.Map Text Text -> [Text] -> M.Map Text Text
      getOpts m [] = m
      getOpts m (t : ts) = let ws = T.words t in if (not . null) ws then getOpts (M.insert (head ws) (T.intercalate " " (tail ws)) m) ts else getOpts m ts
      options = if length ts > 1 then getOpts M.empty (tail ts) else M.empty
   in ((T.strip . head) ts, options)