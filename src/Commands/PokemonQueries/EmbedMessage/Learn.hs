module Commands.PokemonQueries.EmbedMessage.Learn where

import Commands.Utility
import qualified Data.Text as T
import Discord
import Discord.Types
import Pokemon.Types

createLearnEmbed :: T.Text -> [Move] -> [T.Text] -> CreateEmbed
createLearnEmbed pId moves allMoves =
  def
    { createEmbedTitle = T.toTitle pId,
      createEmbedFields =
        [EmbedField "Moves learnable" (if null moves then "Can't learn any of the provided moves" else T.intercalate ", " (map mName moves)) Nothing],
      createEmbedColor = sideColor
    }
  where
    allMovesNotLearnable = getMovesNotLearnable (map (toId . mName) moves) allMoves
    getMovesNotLearnable :: [T.Text] -> [T.Text] -> [T.Text]
    getMovesNotLearnable allMoves [] = []
    getMovesNotLearnable allMoves (m : ms) = if m `notElem` allMoves then m : getMovesNotLearnable allMoves ms else getMovesNotLearnable allMoves ms
    