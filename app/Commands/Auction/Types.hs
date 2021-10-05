module Commands.Auction.Types where

import Data.List
import Data.Maybe
import Data.Text

data User = U {_uName :: Text, _uIdentifier :: Maybe Int} deriving (Eq)

instance Show User where
  show (U name (Just id)) = unpack name ++ "#" ++ show id
  show (U name Nothing) = unpack name

data AuctionID = ID Int Int deriving (Show, Eq)

data Auction = A
  { _aID :: AuctionID,
    _aName :: Text,
    _aMinBid :: Maybe Int,
    _aMinStep :: Maybe Int,
    _aAmountTeam :: Maybe Int,
    _aCurrentBid :: Maybe (User, Item),
    _aAuctioneer :: User,
    _aParticipants :: [Participant]
  }

instance Show Auction where
  show a = auction ++ participants
    where
      auction = "Auction " ++ unpack (_aName a) ++ " has ended, below all the participants and their team!\n\n"
      participants = Data.List.intercalate "\n\n" (Data.List.map show (_aParticipants a))

data Participant = P
  { _pId :: User,
    _pBudget :: Maybe Int,
    _pTeam :: [Item]
  }

instance Show Participant where
  show p = captain ++ "\n" ++ budget ++ "\n" ++ team
    where
      captain = show (_pId p) ++ ":"
      budget = "Remaining Budget: " ++ (show . fromJust . _pBudget) p
      team = Data.List.intercalate "\n" (Data.List.map show (_pTeam p))

data Item = I
  { _iName :: Text,
    _iPrice :: Maybe Int
  }
  deriving (Eq)

instance Show Item where
  show i = unpack (_iName i) ++ " - " ++ (show . fromJust . _iPrice) i

type Auctions = [Auction]

