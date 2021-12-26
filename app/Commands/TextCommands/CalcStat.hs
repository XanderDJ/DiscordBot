module Commands.TextCommands.CalcStat (csCom, msCom) where

import Commands.Parsers
  ( CalcStat (..),
    parseCalcStat,
    parseMaxCalcStat,
    parseOptions,
  )
import Commands.Types (Command (..), CommandFunction (TextCommand))
import Commands.Utility
  ( extractRight,
    getOptionWithDefault,
    ifElse,
    pingUserText,
    sendMessage,
  )
import Data.Either (isLeft)
import qualified Data.Map as M
import Data.StatMultiplier (StatMultiplier, getMultiplier)
import Data.Text (append, pack, unpack)
import Discord (DiscordHandler)
import qualified Discord.Requests as R
import Discord.Types (Message (messageChannel, messageText))
import Pokemon.Functions (calcStat, (*//))
import Text.Parsec (parse)

csCom :: Command
csCom = Com "l(cs|calcstat) ((hp|atk|def|spa|spd|spe):basestat) (positive|neutral|negative|pos|neu|neg) [--level lvl, 100] [--iv iv, 31] [--ev ev, 252] [--(boost|multiplier) -1 0 1, 0]" (TextCommand calcStatHandler)

msCom :: Command
msCom = Com "l(ms|maxstat) (hp|atk|def|spa|spd|spe:basestat) [--(boost|multiplier) -1 0 1, 0]" (TextCommand calcMaxStatHandler)

-- DISCORD HANDLERS

calcStatHandler :: Message -> DiscordHandler ()
calcStatHandler msg = do
  -- parse message
  let (m, opts) = parseOptions (messageText msg)
      pm = parse parseCalcStat "parse calcstat message" m
  ifElse
    (isLeft pm)
    (calcStatUsage msg)
    ( calcStatHandler'
        msg
        (extractRight pm)
        (read . unpack $ getOptionWithDefault "100" ["level", "lvl", "l"] opts)
        (read . unpack $ getOptionWithDefault "31" ["iv", "ivs", "i"] opts)
        (read . unpack $ getOptionWithDefault "252" ["ev", "evs", "e"] opts)
        (fromInteger . read . unpack $ getOptionWithDefault "0" ["multiplier", "boost", "m", "b"] opts)
    )

calcMaxStatHandler :: Message -> DiscordHandler ()
calcMaxStatHandler msg = do
  let (m, opts) = parseOptions (messageText msg)
      pm = parse parseMaxCalcStat "parsing Max calcstat message" m
  ifElse
   (isLeft pm)
   (maxStatUsage msg)
   (calcStatHandler' msg (extractRight pm) 100 31 252 (fromInteger . read . unpack $ getOptionWithDefault "0" ["multiplier", "boost", "m", "b"] opts))

calcStatHandler' :: Message -> CalcStat -> Int -> Int -> Int -> StatMultiplier -> DiscordHandler ()
calcStatHandler' m (CS bs ne) lvl iv ev multiplier = do
  let stat = calcStat lvl iv ev ne bs
  sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) (pack $ ", " ++ show (fromIntegral stat *// getMultiplier multiplier)))

-- ERROR MESSAGES

calcStatUsage :: Message -> DiscordHandler ()
calcStatUsage m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", correct usage: l(cs|calcstat) ((hp|atk|def|spa|spd|spe):basestat) (positive|neutral|negative|pos|neu|neg) [--level lvl, 100] [--iv iv, 31] [--ev ev, 252] [--(boost|multiplier) -1 0 1, 0]")

maxStatUsage :: Message -> DiscordHandler ()
maxStatUsage m = sendMessage $ R.CreateMessage (messageChannel m) (append (pingUserText m) ", correct usage: l(ms|maxstat) ((hp|atk|def|spa|spd|spe):basestat) [--(boost|multiplier) -1 0 1, 0]")
