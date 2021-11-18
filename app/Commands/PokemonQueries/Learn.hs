module Commands.PokemonQueries.Learn where

import Commands.Parsers
import Commands.Types
import Commands.Utility
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Data.Either
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Pokemon.DBConversion (toMove, toPokemon)
import Pokemon.Types
import PokemonDB.Queries
import PokemonDB.Types
import Text.Parsec

learnCom :: Command
learnCom = Com "l(learn|l) (pokemon name), move1, move2, ..." (TextCommand learnCommand)

learnCommand :: Message -> DiscordHandler ()
learnCommand m = do
  let p = parse parseLC "Parsing message for learncommand" (messageText m)
  ifElse (isLeft p) (learnUsage m) (pokemonDb (learnCommand' (extractRight p)) m)

learnCommand' :: (T.Text, [T.Text]) -> Connection -> Message -> DiscordHandler ()
learnCommand' (mon, ms) con m = do
  mon' <- lift $ getCompletePokemon con mon
  moves' <- lift $ getMovesFrom con mon ms
  lift $ close con
  let moves = map toMove moves'
  ifElse (isLeft mon') (invalidMons m [extractLeft mon']) (learnCommand'' (toPokemon . extractRight $ mon') moves ms m)

learnCommand'' :: Pokemon -> [Move] -> [T.Text] -> Message -> DiscordHandler ()
learnCommand'' mon moves allMoves m = void . restCall $ R.CreateMessageEmbed (messageChannel m) "" (createMovesEmbed (pName mon) moves allMoves)

createMovesEmbed :: T.Text -> [Move] -> [T.Text] -> CreateEmbed
createMovesEmbed pId moves allMoves =
  def
    { createEmbedTitle = pId,
      createEmbedFields =
        [ EmbedField "Moves learnable" (if null moves then "Can't learn any of the provided moves" else createMovesText moves) Nothing,
          EmbedField "Moves not learnable" (if null allMovesNotLearnable then "All moves provided can be learned" else T.intercalate ", " allMovesNotLearnable) Nothing
        ]
    }
  where
    allMovesNotLearnable = getMovesNotLearnable (map (toId. mName) moves) allMoves
    getMovesNotLearnable :: [T.Text] -> [T.Text] ->  [T.Text]
    getMovesNotLearnable allMoves [] = []
    getMovesNotLearnable allMoves (m:ms) = if m `notElem` allMoves then  m : getMovesNotLearnable allMoves ms else getMovesNotLearnable allMoves ms
    createMovesText [] = ""
    createMovesText (m : ms) = T.append (T.append (mName m) ", ") (createMovesText ms)

learnUsage :: Message -> DiscordHandler ()
learnUsage = reportError ", ll usage: l(l|learn) (pokemon, i.e. charizard mega x), flame charge, whirlpool, ..."
