{-# LANGUAGE RecordWildCards #-}

module Commands.CursorManager where

import Commands.Cursor
import qualified Data.Map as M
import Data.Maybe
import Data.Text
import Discord.Types
import System.Random

data CursorManager = CursorManager
  { cursorList :: M.Map Text Cursor,
    randomGen :: StdGen
  }
  deriving (Show)

linkMessageId :: CursorManager -> Text -> MessageId -> CursorManager
linkMessageId CursorManager {cursorList = list, ..} key mId = CursorManager list' randomGen
  where
    cursor = M.lookup key list >>= (\c -> return c {cursorMessageId = Just mId})
    list' = if isJust cursor then M.insert key (fromJust cursor) list else list

addCursor :: CursorManager -> Text -> Cursor -> CursorManager
addCursor CursorManager {..} key c = CursorManager (M.insert key c cursorList) randomGen

removeCursor :: CursorManager -> Text -> CursorManager
removeCursor CursorManager {..} key = CursorManager (M.delete key cursorList) randomGen

getCursor :: CursorManager -> Text -> Cursor
getCursor CursorManager {..} key = cursorList M.! key

getDeprecatedCursors :: CursorManager -> UTCTime -> [(Text, Cursor)]
getDeprecatedCursors CursorManager {..} currentTime = M.foldrWithKey (f currentTime) [] cursorList
 where f time key cursor expiredCursors = if isExpired cursor time then (key, cursor) : expiredCursors else expiredCursors 