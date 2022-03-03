{-# LANGUAGE RecordWildCards #-}

module Commands.CursorManager where

import Commands.Cursor
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Data.Text
import qualified Data.Text as T
import Data.Word
import Discord.Interactions
import Discord.Types
import System.Random

data CursorManager = CursorManager
  { cursorList :: M.Map Text Cursor,
    randomGen :: StdGen
  }
  deriving (Show)

linkMessageId :: CursorManager -> Maybe Text -> MessageId -> CursorManager
linkMessageId cm Nothing _ = cm
linkMessageId CursorManager {cursorList = list, ..} (Just key) mId = CursorManager list' randomGen
  where
    cursor = M.lookup key list >>= (\c -> return c {cursorMessageId = Just mId})
    list' = if isJust cursor then M.insert key (fromJust cursor) list else list

hasMessageId :: CursorManager -> Text -> Bool
hasMessageId CursorManager {..} key = fromMaybe False (M.lookup key cursorList <&> (isJust . cursorMessageId))

addCursor :: CursorManager -> Text -> Cursor -> CursorManager
addCursor CursorManager {..} key c = CursorManager (M.insert key c cursorList) randomGen

removeCursor :: CursorManager -> Text -> CursorManager
removeCursor CursorManager {..}  key= CursorManager (M.delete key cursorList) randomGen

getCursor :: CursorManager -> Text -> Cursor
getCursor CursorManager {..} key = cursorList M.! key

getExpiredCursors :: CursorManager -> UTCTime -> [(Text, Cursor)]
getExpiredCursors CursorManager {..} currentTime = M.foldrWithKey (f currentTime) [] cursorList
  where
    f time key cursor expiredCursors = if isExpired cursor time then (key, cursor) : expiredCursors else expiredCursors

getNewKey :: CursorManager -> (T.Text, CursorManager)
getNewKey cm@CursorManager {..} = ((T.pack . show) token, cm {randomGen = newGen})
  where
    (token, newGen) = random randomGen :: (Word64, StdGen)

hasKey :: CursorManager -> Maybe T.Text -> Bool
hasKey _ Nothing = False
hasKey CursorManager {..} (Just k) = M.member k cursorList

isCursorUser :: CursorManager -> MemberOrUser -> T.Text -> Bool
isCursorUser cm (MemberOrUser (Left guildMember)) key = case memberUser guildMember of
  Nothing -> False
  Just user ->
    let cursor = getCursor cm key
     in cursorOwner cursor == userId user
isCursorUser cm (MemberOrUser (Right user)) key =
  let cursor = getCursor cm key
   in cursorOwner cursor == userId user