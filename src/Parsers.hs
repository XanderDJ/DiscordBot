module Parsers where

import Commands.Auction (Auction (A), AuctionID, Participant (P), User (U))
import Data.Functor
import Data.List (isPrefixOf)
import Data.Text (Text, dropWhileEnd, pack, replace, unpack)
import Data.Char ( toLower )
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

parseOutspeed :: Parser (Maybe Int)
parseOutspeed = parseIntCommand "los"

parseOutspeedLevel :: Parser (Maybe Int, Maybe Int)
parseOutspeedLevel = do
  string "losl"
  spaces
  n <- try parseMaybeIntScientific <|> parseMaybeInt
  spaces
  l <- try parseMaybeIntScientific <|> parseMaybeInt
  return (n, l)

