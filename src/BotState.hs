module BotState where

import System.Random

newtype BotState = BotState {
    randomGenerator :: StdGen
}