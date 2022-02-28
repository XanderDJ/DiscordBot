module Commands.PokemonQueries.EmbedMessage.Nature where

import Commands.Utility
import qualified Data.List as L
import Data.Text
import Discord.Types
import Pokemon.Types (Nature (Nature), Stat (NEU))
import Discord
import Discord.Requests
import Data.Maybe

createNatureMessage :: Maybe Nature -> MessageDetailedOpts
createNatureMessage nature = def {messageDetailedEmbeds = Just [createNatureEmbed (fromJust nature)]}

createNatureEmbed :: Nature -> CreateEmbed
createNatureEmbed (Nature name NEU NEU) =
  def
    { createEmbedTitle = pack name,
      createEmbedColor = sideColor,
      createEmbedDescription = "No effect on stats!"
    }
createNatureEmbed (Nature name pos neg) =
  def
    { createEmbedTitle = pack name,
      createEmbedColor = sideColor,
      createEmbedDescription = L.foldl append "10% increase for " [(pack . show) pos, ", 10% decrease for ", (pack . show) neg, "!"]
    }