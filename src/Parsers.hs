module Parsers where

import Auction (Auction (A), AuctionID, Participant (P), User (U))
import Data.Functor
import Data.Text (Text, pack)
import Text.Parsec
  ( alphaNum,
    char,
    digit,
    lookAhead,
    many,
    noneOf,
    spaces,
    string,
    try,
    (<|>),
  )
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
  if null ground || null base
    then return Nothing
    else do
      let x = read base * 1000
          y = read ground * 100
      return $ Just (x + y)

parseUser :: Parser User
parseUser = U . pack <$> many (noneOf "#") <*> (char '#' *> parseMaybeInt)

-- Message example = !auction lpl 5000

registerAuctionP :: AuctionID -> User -> Parser Auction
registerAuctionP aId auctioneer = A aId . pack <$> (string "!hostauction" *> spaces *> many alphaNum) <*> parseMaybeInt <*> parseMaybeInt <*> parseMaybeInt <*> pure Nothing <*> pure auctioneer <*> pure []

-- example: rp mogo#5432 80000

registerParticipantP :: Parser Participant
registerParticipantP = P <$> (string "rp" *> spaces *> parseUser) <*> parseMaybeInt <*> pure []

-- example: nom jeroni9999
nominatePlayerP :: Parser Text
nominatePlayerP = pack <$> (string "nom" *> spaces *> many alphaNum)

-- example: b (5000|5.0k|5.0|5k)
bidP :: Parser (Maybe Int)
bidP = string "b" *> spaces *> (try parseMaybeIntScientific <|> parseMaybeInt)

-- example: info lonewulfx3#3333
infoP :: Parser User
infoP = U . pack <$> (string "info" *> spaces *> many (noneOf "#")) <*> (char '#' *> parseMaybeInt)