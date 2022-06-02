module Commands.PokemonQueries.PokemonQueries where

import BotState (BotState (cursorManager))
import Commands.Cursor (Cursor (Cursor, InvalidCursor), createCursorMessage)
import Commands.CursorManager
import Commands.PaginatedContents
import Commands.Parsers
import Commands.PokemonQueries.EmbedMessage.Ability
import Commands.PokemonQueries.EmbedMessage.Item
import Commands.PokemonQueries.EmbedMessage.Learn
import Commands.PokemonQueries.EmbedMessage.Move (createMoveEmbed)
import Commands.PokemonQueries.EmbedMessage.Nature
  ( createNatureMessage,
  )
import Commands.PokemonQueries.EmbedMessage.Pokemon (createPokemonEmbed)
import Commands.Types
import Commands.Utility
import Control.Concurrent
import Control.Monad.Trans
import Data.Either
import Data.Function
import qualified Data.Map as M
import Data.Maybe
import Data.Text (justifyRight)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import Discord
import Discord.Requests
import qualified Discord.Requests as R
import Discord.Types
import Pokemon.DBConversion (toDTType, toMove, toPokemon)
import Pokemon.Functions hiding (toId)
import Pokemon.Nature
import Pokemon.Types
import PokemonDB.Queries
import PokemonDB.Types
import Text.Parsec
import Text.Pretty.Simple (pPrint)
import Discord.Interactions

queryCom :: Command
queryCom = Com "To be defined" (CursorCommand parseQuery)

-- | First parse query
-- -> If succesfull open connection to db and run query
-- -> Transform QueryResult into Discord embedded message, optionally figure out how to use commandtabs
parseQuery :: MVar CursorManager -> Message -> DiscordHandler ()
parseQuery cursorManagerVar msg = do
  let (parsableContent, opts) = combineOptionsWith (combineWithSep ",") (parseOptionsOnSep ',' (messageContent msg))
      query = parse queryParser "Parsing a message to a query" parsableContent
  ifElse (isLeft query) (reportError "Couldn't parse the given query, a better help system will be implemented eventually" msg) (preProcessQuery (extractRight query) opts cursorManagerVar msg)

preProcessQuery :: PokemonQuery -> Options -> MVar CursorManager -> Message -> DiscordHandler ()
preProcessQuery query opts cursorManagerVar msg = do
  let message = quitEarlyMessage query opts
      quitEarly = isJust message
  ifElse quitEarly (sendMessage $ R.CreateMessageDetailed (messageChannelId msg) (fromJust message)) (pokemonDb (getQueryResult query opts cursorManagerVar) msg)

getQueryResult :: PokemonQuery -> Options -> MVar CursorManager -> Connection -> Message -> DiscordHandler ()
getQueryResult query opts cursorManagerVar con msg = do
  queryResult <- lift $ runPokemonQuery con query
  lift $ close con
  parseQueryResult query queryResult opts cursorManagerVar msg

parseQueryResult :: PokemonQuery -> PokemonQueryResult -> Options -> MVar CursorManager -> Message -> DiscordHandler ()
parseQueryResult originalQuery queryResult opts cursorManagerVar msg = do
  let requiresCursor = createsCursor queryResult
  ifElse requiresCursor (cursor originalQuery queryResult opts cursorManagerVar msg) (noCursor originalQuery queryResult opts cursorManagerVar msg)

cursor :: PokemonQuery -> PokemonQueryResult -> Options -> MVar CursorManager -> Message -> DiscordHandler ()
cursor originalQuery queryResult opts cursorManagerVar msg = do
  time <- lift getCurrentTime
  let c = createCursor originalQuery queryResult opts msg time
  if c == InvalidCursor
    then sendMessage $ R.CreateMessage (messageChannelId msg) (T.append (pingUserText msg) ", the given query didn't offer any results or wasn't implemented yet!")
    else do
      cursorManager <- lift $ takeMVar cursorManagerVar
      let (key, cm') = getNewKey cursorManager
          cm'' = addCursor cm' key c
      lift $ putMVar cursorManagerVar cm''
      let detailedMessage = createCursorMessage c key
      sendMessage $ R.CreateMessageDetailed (messageChannelId msg) detailedMessage

noCursor :: PokemonQuery -> PokemonQueryResult -> M.Map T.Text T.Text -> MVar CursorManager -> Message -> DiscordHandler ()
noCursor originalQuery queryResult opts cursorManagerVar msg = do
  let discordMessage = createDiscordMessage originalQuery queryResult opts msg
  sendMessage $ R.CreateMessageDetailed (messageChannelId msg) discordMessage

-- | Check wether a result has to be put into a cursor or not
createsCursor :: PokemonQueryResult -> Bool
createsCursor pqr = case pqr of
  -- Only list patterns that don't create a cursor since these are less common
  LearnR mts -> False
  DTR m_dd -> False
  other -> True

quitEarlyMessage :: PokemonQuery -> Options -> Maybe R.MessageDetailedOpts
quitEarlyMessage (DT maybeNature) _ =
  let nature = getNature maybeNature
   in if isJust nature then Just $ createNatureMessage nature else Nothing
quitEarlyMessage query opts = Nothing

createDiscordMessage :: PokemonQuery -> PokemonQueryResult -> Options -> Message -> MessageDetailedOpts
createDiscordMessage (DT oId) (DTR mdt) opts msg = case mdt of
  Nothing ->
    -- dt could be nature
    def {R.messageDetailedContent = T.append (pingUserText msg) ", couldn't find the data you were looking for."}
  Just dt -> case toDTType dt of
    DtPokemon pokemon -> createDetailedMessage $ createPokemonEmbed pokemon opts
    DtItem item -> createDetailedMessage $ createItemEmbed item
    DtMove move -> createDetailedMessage $ createMoveEmbed move
    DtAbility ability -> createDetailedMessage $ createAbilityEmbed ability
    other -> def {messageDetailedContent = "Encountered unreachable situation"}
createDiscordMessage (Learn pId allMoves) (LearnR learnableMoves) _ _ = createDetailedMessage $ createLearnEmbed pId (map toMove learnableMoves) (map toId allMoves)
createDiscordMessage pq pqr opts msg = def {R.messageDetailedContent = "Not implemented yet"}

createCursor :: PokemonQuery -> PokemonQueryResult -> M.Map T.Text T.Text -> Message -> UTCTime -> Cursor
createCursor (AllMovesFromType pId moveType) (AllMovesFromTypeR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All " ++ (T.unpack . T.toTitle) moveType ++ " moves that " ++ (T.unpack . T.toTitle) pId ++ " can learn.")
    time
    msg
createCursor (AllMovesFromCategory pId moveCategory) (AllMovesFromCategoryR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All " ++ (T.unpack . T.toTitle) moveCategory ++ " moves that " ++ (T.unpack . T.toTitle) pId ++ " can learn.")
    time
    msg
createCursor (AllMovesFromCategoryAndType pId mCat mType) (AllMovesFromCategoryAndTypeR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All " ++ (T.unpack . T.toTitle) mCat ++ "  " ++ (T.unpack . T.toTitle) mType ++ " moves that " ++ (T.unpack . T.toTitle) pId ++ " can learn.")
    time
    msg
createCursor (AllMoves pId) (AllMovesR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All moves that " ++ (T.unpack . T.toTitle) pId ++ " can learn.")
    time
    msg
createCursor (AllPriorityMoves pId) (AllPriorityMovesR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All priority moves that " ++ (T.unpack . T.toTitle) pId ++ " can learn.")
    time
    msg
createCursor (AllHazardMoves pId) (AllHazardMovesR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All hazards that " ++ (T.unpack . T.toTitle) pId ++ " can set up.")
    time
    msg
createCursor (AllHazardControl pId) (AllHazardControlR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All hazard control that " ++ (T.unpack . T.toTitle) pId ++ " has.")
    time
    msg
createCursor (AllRecoveryMoves pId) (AllRecoveryMovesR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All recovery moves that " ++ (T.unpack . T.toTitle) pId ++ " can learn.")
    time
    msg
createCursor (AllClericMoves pId) (AllClericMovesR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All cleric moves that " ++ (T.unpack . T.toTitle) pId ++ " can learn.")
    time
    msg
createCursor (AllScreens pId) (AllScreensR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All screens that " ++ (T.unpack . T.toTitle) pId ++ " can learn.")
    time
    msg
createCursor (AllSetUpMoves pId) (AllSetUpMovesR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All set up moves that " ++ (T.unpack . T.toTitle) pId ++ " can learn.")
    time
    msg
createCursor (AllStatusMoves pId) (AllStatusMovesR dbMoves) _ msg time =
  movesToCursor
    (map toMove dbMoves)
    "Query results"
    (T.pack $ "All status inducing moves that " ++ (T.unpack . T.toTitle) pId ++ " can learn.")
    time
    msg
createCursor (AllPokemonsWithAbility aId) (AllPokemonsWithAbilityR dbMons) _ msg time =
  pokemonsToCursor
    (map toPokemon dbMons)
    "Query results"
    (T.pack $ "All pokemons that can have the " ++ (T.unpack . T.toTitle) aId ++ " ability.")
    time
    msg
createCursor (AllPokemonsWithMove mId) (AllPokemonsWithMoveR dbMons) _ msg time =
  pokemonsToCursor
    (map toPokemon dbMons)
    "Query results"
    (T.pack $ "All pokemons that can learn " ++ (T.unpack . T.toTitle) mId ++ ".")
    time
    msg
createCursor (AllPokemonWithStat stat value) (AllPokemonWithStatR dbMons) _ msg time =
  pokemonsToCursor
    (map toPokemon dbMons)
    "Query results"
    (T.pack $ "All pokemons that have " ++ show value ++ " " ++ (T.unpack . T.toTitle) stat ++ ".")
    time
    msg
createCursor originalQuery queryResult options msg time = InvalidCursor

pokemonsToCursor :: [Pokemon] -> T.Text -> T.Text -> UTCTime -> Message -> Cursor
pokemonsToCursor mons title desc time msg =
  if null mons
    then InvalidCursor
    else
      Cursor
        0
        (max 0 (div (length mons - 1) 8))
        Nothing
        (Just $ messageChannelId msg)
        8
        (PaginatedContents title desc (pokemonsToFieldMap mons))
        time
        ((userId . messageAuthor) msg)

pokemonsToFieldMap :: [Pokemon] -> [(T.Text, [T.Text])]
pokemonsToFieldMap mons =
  [ ("Name", map pName mons),
    ("Typing", map (T.intercalate " " . map (typeToEmote . T.pack . show) . pTyping) mons),
    ("HP   ATK  DEF  SPA  SPD  SPE  ", map (T.pack . toShortString . baseStats) mons)
  ]
  where
    toShortString :: BaseStats -> String
    toShortString bs = f bs HP ++ " " ++ f bs ATK ++ " " ++ f bs DEF ++ " " ++ f bs SPA ++ " " ++ f bs SPD ++ " " ++ f bs SPE
    f bs stat = (show . getValue . findBaseStat bs) stat

movesToCursor :: [Move] -> T.Text -> T.Text -> UTCTime -> Message -> Cursor
movesToCursor moves title desc time msg =
  if null moves
    then InvalidCursor
    else
      Cursor
        0
        (max 0 (div (length moves - 1) 8))
        Nothing
        (Just $ messageChannelId msg)
        8
        (PaginatedContents title desc (movesToFieldMap moves))
        time
        ((userId . messageAuthor) msg)

movesToFieldMap :: [Move] -> [(T.Text, [T.Text])]
movesToFieldMap moves =
  [ ("Name", map mName moves),
    ("BP/PP", map (\m -> T.pack $ (show . fromMaybe 0 . mBp) m ++ "/" ++ (show . mPP) m) moves),
    ("Accuracy", map (\m -> T.append ((T.pack . show . fromMaybe 0 . mAccuracy) m) "%") moves)
  ]

interactionPing :: Interaction -> MemberOrUser
interactionPing = interactionUser

usertest :: MemberOrUser
usertest = interactionPing (InteractionPing 12 332 "" 1)