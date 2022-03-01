module BotState where

import System.Random
import Commands.Auction
import Control.Concurrent
import Commands.CursorManager (CursorManager)

data BotState = BotState {
    auctionState :: MVar Auctions,
    cursorManager :: MVar CursorManager
}