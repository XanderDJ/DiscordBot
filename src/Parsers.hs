module Parsers where

import Commands.Auction (Auction (A), AuctionID, Participant (P), User (U))
import Data.Functor
import Data.Text (Text, dropWhileEnd, pack, unpack)
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
bidP = string "lb" *> spaces *> (try parseMaybeIntScientific <|> parseMaybeInt)

-- example: info lonewulfx3#3333
infoP :: Parser User
infoP = U . pack <$> (string "linfouser" *> spaces *> many (noneOf "#")) <*> (char '#' *> parseMaybeInt)

undoP :: Parser User
undoP = U . pack <$> (string "lundo" *> spaces *> many (noneOf "#")) <*> (char '#' *> parseMaybeInt)

dtP :: Parser String
dtP = map (\s -> if s == ' ' then '-' else s) <$> (string "ldt " *> spaces *> many anyChar)