module Commands.PokemonQueries.EmbedMessage.Item where

import Commands.Utility
import qualified Data.List as L
import Data.Text
import Discord
import Discord.Types
import Pokemon.Types

createItemEmbed :: Item -> CreateEmbed
createItemEmbed (Item _ name (Just desc) _ flingBp _ _ _) =
  def
    { createEmbedTitle = L.foldl append name [" (flingBP=", pack . show $ flingBp, ")"],
      createEmbedThumbnail = (Just . CreateEmbedImageUrl . getItemUrl) name,
      createEmbedColor = sideColor,
      createEmbedDescription = desc
    }
createItemEmbed (Item _ name Nothing _ flingBp _ _ _) = def {createEmbedTitle = L.foldl append name [" (flingBP=", pack . show $ flingBp, ")"], createEmbedDescription = "No description for this item yet!"}

getItemUrl :: Text -> Text
getItemUrl name = L.foldl append "https://www.serebii.net/itemdex/sprites/pgl/" [toId name, ".png"]