module Commands.TextCommands.SpeedCommands (osCom) where

import Commands.Types (Command (..), CommandFunction (TextCommand))
import Commands.Utility (ifElse, pingUserText, sendMessage)
import Data.Either (fromRight, isLeft)
import Data.Maybe (fromJust, isNothing)
import Data.Text (append, pack)
import Discord (DiscordHandler)
import qualified Discord.Requests as R
import Discord.Types
import Commands.Parsers (parseOutspeedLevel)
import Pokemon.Functions (maxStatAt, neutralMaxStatAt, (*//))
import Pokemon.Types 
import Text.Parsec (parse)

-- OUTSPEED COMMAND

osCom :: Command
osCom = Com "los (speed) (level) - Gives the basespeeds a pokemon at level l (100 if none given) needs to outspeed the given speed!" (TextCommand outspeedLevelCommand)

outspeedLevelCommand :: Message -> DiscordHandler ()
outspeedLevelCommand m = do
  let r = parse parseOutspeedLevel "Parsing speed and level" (messageContent m)
  ifElse (isLeft r) (outspeedLevelUsage m) (outspeedLevelCommand' m (fromRight (Nothing, Nothing) r))

outspeedLevelCommand' :: Message -> (Maybe Int, Maybe Level) -> DiscordHandler ()
outspeedLevelCommand' m (mi, ml) = ifElse (isNothing mi) (outspeedLevelUsage m) (ifElse (isNothing ml) (outspeedCommand'' m (fromJust mi) 100) (outspeedCommand'' m (fromJust mi) (fromJust ml)))

outspeedCommand'' :: Message -> Int -> Level -> DiscordHandler ()
outspeedCommand'' m i l = do
  let s = toSpeedText i $ outspeed i l
  sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) (append ", " (pack s)))

-- ERROR MESSAGES

outspeedLevelUsage :: Message -> DiscordHandler ()
outspeedLevelUsage m = sendMessage $ R.CreateMessage (messageChannelId m) (append (pingUserText m) ", correct usage: losl (speed) (lvl, 100 if not given)")

-- HELPER FUNCTIONS

-- | Calculate the base speeds needed to outspeed the given speed stat.
--   Result returns (outspeed with max speed neutral nature, outspeed with scarf max speed neutral nature, outspeed with max speed, outspeed with scarf max speed)
outspeed :: Int -> Level -> (Int, Int, Int, Int)
outspeed i lvl = go i 1 lvl Nothing Nothing Nothing Nothing
  where
    go :: Int -> Int -> Level -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> (Int, Int, Int, Int)
    go i bs lvl Nothing Nothing Nothing Nothing = if fromIntegral (maxStatAt lvl (BaseStat SPE bs)) *// 1.5 > i then go i bs lvl Nothing Nothing Nothing (Just bs) else go i (bs + 1) lvl Nothing Nothing Nothing Nothing
    go i bs lvl Nothing Nothing Nothing (Just ms) = if fromIntegral (neutralMaxStatAt lvl (BaseStat SPE bs)) *// 1.5 > i then go i bs lvl Nothing (Just bs) Nothing (Just ms) else go i (bs + 1) lvl Nothing Nothing Nothing (Just ms)
    go i bs lvl Nothing (Just ns) Nothing ms = if maxStatAt lvl (BaseStat SPE bs) > i then go i bs lvl Nothing (Just ns) (Just bs) ms else go i (bs + 1) lvl Nothing (Just ns) Nothing ms
    go i bs lvl Nothing ns (Just m) ms = if neutralMaxStatAt lvl (BaseStat SPE bs) > i then go i bs lvl (Just bs) ns (Just m) ms else go i (bs + 1) lvl Nothing ns (Just m) ms
    go i bs lvl (Just n) (Just ns) (Just m) (Just ms) = (n, ns, m, ms)
    go i bs lvl _ _ _ _ = error "Unachievable situation reached while calculating minimum basespeeds to outspeed a certain speed stat"

toSpeedText :: Int -> (Int, Int, Int, Int) -> String
toSpeedText s (n, ns, m, ms) =
  "To outspeed " ++ show s ++ " speed:\n"
    ++ "With scarf max speed, you need "
    ++ show ms
    ++ " base speed!\n"
    ++ "With scarf neutral speed, you need "
    ++ show ns
    ++ " base speed!\n"
    ++ "With max speed, you need "
    ++ show m
    ++ " base speed!\n"
    ++ "With max neutral speed, you need "
    ++ show n
    ++ " base speed!"