{-# LANGUAGE RecordWildCards #-}

module Commands.Cursor where

import Commands.ActionRow (searchActionRow)
import Commands.PaginatedContents
import Control.Arrow (Arrow (second))
import qualified Data.Text as T
import Data.Time.Clock
import Discord
import Discord.Requests
import Discord.Types
import Commands.Utility (sideColor)

data Cursor
  = Cursor
      { cursorPage :: Int,
        cursorTotalPages :: Int,
        cursorMessageId :: Maybe Snowflake,
        cursorChannelId :: Maybe Snowflake,
        cursorSize :: Int,
        cursorContent :: PaginatedContents,
        cursorLastAccessed :: UTCTime
      }
  | InvalidCursor
  deriving (Show)

isExpired :: Cursor -> UTCTime -> Bool
isExpired Cursor {cursorLastAccessed = lastAccessed} time = diffUTCTime lastAccessed time > 10
isExpired InvalidCursor _ = True

nextPage :: Cursor -> Cursor
nextPage c@Cursor {..} =
  let newPage = cursorPage + 1
   in c {cursorPage = if newPage > cursorTotalPages then cursorTotalPages else newPage}
nextPage InvalidCursor = InvalidCursor

previousPage :: Cursor -> Cursor
previousPage c@Cursor {..} =
  let newPage = cursorPage - 1
   in c {cursorPage = if newPage < 0 then 0 else newPage}
previousPage InvalidCursor = InvalidCursor

createCursorMessage :: Cursor -> T.Text -> MessageDetailedOpts
createCursorMessage c@Cursor {..} key =
  def
    { messageDetailedEmbeds =
        Just
          [ def
              { createEmbedTitle = paginatedTitle cursorContent,
                createEmbedDescription = paginatedText cursorContent,
                createEmbedFields = createCursorFields c,
                createEmbedColor = sideColor
              }
          ],
      messageDetailedComponents = Just [searchActionRow key False]
    }
createCursorMessage InvalidCursor key = def

createCursorFields :: Cursor -> [EmbedField]
createCursorFields Cursor {..} =
  let fieldsToShow = map (second (take cursorSize . drop (cursorPage * cursorSize))) (paginatedFieldsMap cursorContent)
      toEmbedField (title, contents) = EmbedField title (T.intercalate "\n" contents) (Just True)
   in map toEmbedField fieldsToShow
createCursorFields InvalidCursor = []