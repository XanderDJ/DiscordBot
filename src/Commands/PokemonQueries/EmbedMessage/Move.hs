{-# LANGUAGE RecordWildCards #-}

module Commands.PokemonQueries.EmbedMessage.Move where

import Commands.Utility
import Data.Maybe
import Data.Text hiding (null)
import Discord
import Discord.Types
import Pokemon.Functions
import Pokemon.Types

createMoveEmbed :: Move -> CreateEmbed
createMoveEmbed move =
  def
    { createEmbedTitle = moveTitle move,
      createEmbedColor = sideColor,
      createEmbedDescription = if isNothing (mDescription move) then "" else fromJust . mDescription $ move,
      createEmbedFields = [EmbedField "Stats" (getStatsText move) (Just True), EmbedField "Flags" (if null (mFlags move) then "No flags" else intercalate "\n" (mFlags move)) (Just True)]
    }

moveTitle :: Move -> Text
moveTitle Move {..} = pack (unpack mName ++ " (Prio=" ++ show mPrio ++ ")")

getStatsText :: Move -> Text
getStatsText move = intercalate "\n" [typeText, dmgClassText, bpText, accuracyText, ppText]
  where
    bpText = if isNothing (mBp move) then "**BP**: 0" else append "**BP: ** " (pack . show . fromJust . mBp $ move)
    accuracyText = if isNothing (mAccuracy move) then "**Accuracy**: never misses" else append "**Accuracy: ** " (pack . show . fromJust . mAccuracy $ move)
    ppText = pack . ("**PP: **" ++) . show . (*// (8 / 5)) . fromIntegral . mPP $ move
    typeText = pack . show . mTipe $ move
    dmgClassText = pack . show . mDClass $ move