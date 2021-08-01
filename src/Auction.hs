module Auction where

import Data.Text

data User = U {_uName ::Text, _uIdentifier :: Maybe Int} deriving Eq

instance Show User where
  show (U name (Just id)) = unpack name ++ "#" ++ show id
  show (U name Nothing) = unpack name

data AuctionID = ID Int Int deriving (Show, Eq)

data Auction = A {
  _aID :: AuctionID,
  _aName :: Text,
  _aMinBid :: Maybe Int,
  _aMinStep :: Maybe Int,
  _aAmountTeam :: Maybe Int,
  _aCurrentBid :: Maybe (User, Item),
  _aAuctioneer :: User,
  _aParticipants :: [Participant]
} deriving Show

data Participant = P {
  _pId :: User,
  _pBudget :: Maybe Int,
  _pLatestBid :: Maybe Int, -- necessary for when participant has won auction
  _pTeam :: [Item]
} deriving Show

data Item = I {
  _iName :: Text,
  _iPrice :: Maybe Int
} deriving (Show, Eq)

type Auctions = [Auction]

getAuction :: AuctionID -> [Auction] -> Maybe Auction
getAuction _ [] = Nothing
getAuction id (a:as) = if id == _aID a then Just a else getAuction id as  