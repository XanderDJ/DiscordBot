module Interactions.CursorInteraction where

import Commands.Cursor
import Commands.CursorManager
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import Data.Time
import Discord
import Discord.Interactions
import qualified Discord.Requests as R
import Text.Pretty.Simple
import Utility (parseToken)

handleCursorInteraction :: T.Text -> MVar CursorManager -> Interaction -> DiscordHandler ()
handleCursorInteraction customId cmv i = do
  cursorManager <- lift $ takeMVar cmv
  now <- lift getCurrentTime
  let (Just key, action) = parseCustomId customId
      cursor = getCursor cursorManager key
      newCursor = (cursorAccessed now . takeAction action) cursor
  when (action /= Delete && action /= NoAction) (updateCursor key newCursor cursorManager cmv i)
  when (action == Delete) (deleteCursor key cursorManager cmv i)
  when (action == NoAction) (lift $ putMVar cmv cursorManager)

updateCursor :: T.Text -> Cursor -> CursorManager -> MVar CursorManager -> Interaction -> DiscordHandler ()
updateCursor key newCursor cursorManager cmv i = do
  let interactionResponse = createCursorInteractionResponse newCursor
      cm' = addCursor cursorManager key newCursor
  void . restCall $ R.CreateInteractionResponse (interactionId i) (interactionToken i) (InteractionResponseUpdateMessage interactionResponse)
  lift $ putMVar cmv cm'

deleteCursor :: T.Text -> CursorManager -> MVar CursorManager -> Interaction -> DiscordHandler ()
deleteCursor key cm cmv i = do
  let cm' = removeCursor cm key
  void . restCall $
    R.CreateInteractionResponse
      (interactionId i)
      (interactionToken i)
      ( InteractionResponseUpdateMessage
          ( InteractionResponseMessage
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              (Just [])
              Nothing
          )
      )
  lift $ putMVar cmv cm'

readAction :: T.Text -> CursorAction
readAction "first" = First
readAction "back" = Back
readAction "delete" = Delete
readAction "forward" = Forward
readAction "last" = Last
readAction other = NoAction

parseCustomId customId = (parseToken customId, (readAction . T.toLower . T.takeWhileEnd (/= ' ')) customId)