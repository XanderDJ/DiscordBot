{-# LANGUAGE RecordWildCards #-}

module Commands.Cursor where

import Data.Time.Clock
import Discord.Types
import Commands.PaginatedContents
import Discord.Requests (MessageDetailedOpts)

data Cursor = Cursor
  { cursorPage :: Int,
    cursorTotalPages :: Int,
    cursorMessageId :: Maybe Snowflake,
    cursorChannelId :: Maybe Snowflake,
    cursorSize :: Int,
    cursorContent :: PaginatedContents,
    cursorLastAccessed :: UTCTime
  } deriving Show

isExpired :: Cursor -> UTCTime -> Bool
isExpired Cursor {cursorLastAccessed = lastAccessed} time = diffUTCTime lastAccessed time > 10

nextPage :: Cursor -> Cursor
nextPage c@Cursor {..} = c {cursorPage = if newPage > cursorTotalPages then cursorTotalPages else newPage}
  where
    newPage = cursorPage + 1

previousPage :: Cursor -> Cursor
previousPage c@Cursor {..} = c {cursorPage = if newPage < 0 then 0 else newPage}
  where
    newPage = cursorPage - 1


createCursorMessage :: Cursor -> MessageDetailedOpts
createCursorMessage c = undefined