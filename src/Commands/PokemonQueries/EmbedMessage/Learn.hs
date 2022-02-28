module Commands.PokemonQueries.EmbedMessage.Learn where

import Commands.Utility
import qualified Data.Text as T
import Discord
import Discord.Types
import Pokemon.Types

createLearnEmbed :: T.Text -> [Move] -> [T.Text] -> CreateEmbed
createLearnEmbed pId moves allMoves =
  def
    { createEmbedTitle = pId,
      createEmbedFields =
        [EmbedField "Moves learnable" (if null moves then "Can't learn any of the provided moves" else T.intercalate ", " (map mName moves)) Nothing],
      createEmbedColor = sideColor
    }
