module Commands.PokemonQueries.PokemonQueries where

import Commands.Parsers
import Commands.PokemonQueries.EmbedMessage.Ability
import Commands.PokemonQueries.EmbedMessage.Item
import Commands.PokemonQueries.EmbedMessage.Learn (createLearnEmbed)
import Commands.PokemonQueries.EmbedMessage.Move (createMoveEmbed)
import Commands.PokemonQueries.EmbedMessage.Nature
import Commands.PokemonQueries.EmbedMessage.Pokemon (createPokemonEmbed)
import Commands.Types
import Commands.Utility
import Control.Monad.Trans
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Discord
import Discord.Requests
import qualified Discord.Requests as R
import Discord.Types
import Pokemon.DBConversion (toDTType, toMove)
import Pokemon.Nature
import Pokemon.Types
import PokemonDB.Queries
import PokemonDB.Types
import Text.Parsec

queryCom :: Command
queryCom = Com "To be defined" (TextCommand parseQuery)

-- | First parse query
-- -> If succesfull open connection to db and run query
-- -> Transform QueryResult into Discord embedded message, optionally figure out how to use commandtabs
parseQuery :: Message -> DiscordHandler ()
parseQuery m = do
  let (parsableContent, opts) = combineOptionsWith (combineWithSep ",") (parseOptionsOnSep ',' (messageContent m))
      query = parse queryParser "Parsing a message to a query" parsableContent
  ifElse (isLeft query) (reportError "Couldn't parse the given query, a better help system will be implemented eventually" m) (preProcessQuery (extractRight query) opts m)

preProcessQuery :: PokemonQuery -> Options -> Message -> DiscordHandler ()
preProcessQuery query opts m = do
  let message = quitEarlyMessage query opts
      quitEarly = isJust message
  ifElse quitEarly (sendMessage $ R.CreateMessageDetailed (messageChannelId m) (fromJust message)) (pokemonDb (getQueryResult query opts) m)

getQueryResult :: PokemonQuery -> Options -> Connection -> Message -> DiscordHandler ()
getQueryResult query opts con m = do
  queryResult <- lift $ runPokemonQuery con query
  lift $ close con
  parseQueryResult query queryResult opts m

parseQueryResult :: PokemonQuery -> PokemonQueryResult -> Options -> Message -> DiscordHandler ()
parseQueryResult originalQuery queryResult opts m = do
  let discordMessage = createDiscordMessage originalQuery queryResult opts m
  sendMessage $ R.CreateMessageDetailed (messageChannelId m) discordMessage

quitEarlyMessage :: PokemonQuery -> Options -> Maybe R.MessageDetailedOpts
quitEarlyMessage (DT maybeNature) _ =
  let nature = getNature maybeNature
   in if isJust nature then Just $ createNatureMessage nature else Nothing
quitEarlyMessage query opts = Nothing

createDiscordMessage :: PokemonQuery -> PokemonQueryResult -> Options -> Message -> MessageDetailedOpts
createDiscordMessage (DT oId) (DTR mdt) opts m = case mdt of
  Nothing ->
    -- dt could be nature
    def {R.messageDetailedContent = T.append (pingUserText m) ", couldn't find the data you were looking for."}
  Just dt -> case toDTType dt of
    DtPokemon pokemon -> createDetailedMessage $ createPokemonEmbed pokemon opts
    DtItem item -> createDetailedMessage $ createItemEmbed item
    DtMove move -> createDetailedMessage $ createMoveEmbed move
    DtAbility ability -> createDetailedMessage $ createAbilityEmbed ability
    other -> def {messageDetailedContent = "Encountered unreachable situation"}
createDiscordMessage (Learn pId allMoves) (LearnR learnableMoves) _ _ = createDetailedMessage $ createLearnEmbed pId (map toMove learnableMoves) (map toId allMoves)
createDiscordMessage pq pqr opts m = def {R.messageDetailedContent = "Not implemented yet"}