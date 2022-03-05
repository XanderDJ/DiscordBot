{-# LANGUAGE RecordWildCards #-}

module Commands.Cursor where

import Commands.ActionRow (searchActionRow)
import Commands.PaginatedContents
import Commands.Utility (sideColor)
import Control.Arrow (Arrow (second))
import qualified Data.Text as T
import Data.Time.Clock
import Discord
import Discord.Interactions
import Discord.Requests
import Discord.Types

data Cursor
  = Cursor
      { cursorPage :: Int,
        cursorTotalPages :: Int,
        cursorMessageId :: Maybe Snowflake,
        cursorChannelId :: Maybe Snowflake,
        cursorSize :: Int,
        cursorContent :: PaginatedContents,
        cursorLastAccessed :: UTCTime,
        cursorOwner :: UserId
      }
  | InvalidCursor
  deriving (Show, Eq)

isExpired :: Cursor -> UTCTime -> Bool
isExpired Cursor {cursorLastAccessed = lastAccessed} time = diffUTCTime time lastAccessed > 10
isExpired InvalidCursor _ = True

firstPage :: Cursor -> Cursor
firstPage c@Cursor {..} = c {cursorPage = 0}
firstPage InvalidCursor = InvalidCursor

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

lastPage :: Cursor -> Cursor
lastPage c@Cursor {..} = c {cursorPage = cursorTotalPages}
lastPage InvalidCursor = InvalidCursor

cursorAccessed :: UTCTime -> Cursor -> Cursor
cursorAccessed _ InvalidCursor = InvalidCursor
cursorAccessed now c = c {cursorLastAccessed = now}

createCursorMessage :: Cursor -> T.Text -> MessageDetailedOpts
createCursorMessage c@Cursor {..} key =
  def
    { messageDetailedEmbeds =
        Just
          [ def
              { createEmbedTitle = paginatedTitle cursorContent,
                createEmbedDescription = paginatedText cursorContent,
                createEmbedFields = createCursorFields c,
                createEmbedFooterText = T.pack $ "Page " ++ show (cursorPage + 1) ++ "/" ++ show (cursorTotalPages + 1),
                createEmbedColor = sideColor
              }
          ],
      messageDetailedComponents = Just [searchActionRow key False]
    }
createCursorMessage InvalidCursor key = def

createCursorInteractionResponse :: Cursor -> InteractionResponseMessage
createCursorInteractionResponse c@Cursor {..} =
  InteractionResponseMessage
    Nothing
    Nothing
    ( Just
        [ def
            { createEmbedTitle = paginatedTitle cursorContent,
              createEmbedDescription = paginatedText cursorContent,
              createEmbedFields = createCursorFields c,
              createEmbedFooterText = T.pack $ "Page " ++ show (cursorPage + 1) ++ "/" ++ show (cursorTotalPages + 1),
              createEmbedColor = sideColor
            }
        ]
    )
    Nothing
    Nothing
    Nothing
    Nothing
createCursorInteractionResponse other =
  InteractionResponseMessage
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

createCursorFields :: Cursor -> [EmbedField]
createCursorFields Cursor {..} =
  let fieldsToShow = map (second (take cursorSize . drop (cursorPage * cursorSize))) (paginatedFieldsMap cursorContent)
      toEmbedField (title, contents) = EmbedField title (T.intercalate "\n" contents) (Just True)
   in map toEmbedField fieldsToShow
createCursorFields InvalidCursor = []

data CursorAction = First | Back | Delete | Forward | Last | NoAction deriving (Show, Eq)

takeAction :: CursorAction -> Cursor -> Cursor
takeAction First = firstPage
takeAction Back = previousPage
takeAction Forward = nextPage
takeAction Last = lastPage
takeAction other = id
