module Commands.PokemonQueries.EmbedMessage.Ability where
import Pokemon.Types
import Discord.Types
import Discord
import Commands.Utility


createAbilityEmbed :: Ability -> CreateEmbed
createAbilityEmbed (Ability _ name (Just desc)) =
  def
    { createEmbedTitle = name,
      createEmbedColor = sideColor,
      createEmbedDescription = desc
    }
createAbilityEmbed (Ability _ name Nothing) = def {createEmbedTitle = name, createEmbedDescription = "No description for this ability yet!"}