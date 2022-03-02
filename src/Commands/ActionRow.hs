module Commands.ActionRow where

import qualified Data.Text as T
import Discord
import Discord.Types

searchActionRow :: T.Text -> Bool -> ComponentActionRow
searchActionRow key b =
  ComponentActionRowButton
    [ ComponentButton (T.append key " first") b ButtonStyleSecondary Nothing (Just (mkEmoji "⏪")),
      ComponentButton (T.append key " back") b ButtonStyleSecondary Nothing (Just (mkEmoji "◀️")),
      ComponentButton (T.append key " delete") b ButtonStyleSecondary Nothing (Just (mkEmoji "⏹️")),
      ComponentButton (T.append key " forward") b ButtonStyleSecondary Nothing (Just (mkEmoji "▶️")),
      ComponentButton (T.append key " last") b ButtonStyleSecondary Nothing (Just (mkEmoji "⏩"))
    ]

