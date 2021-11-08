module Pokemon.Excel (
  speedTable,
  pokemonMoveMap
 ) where

import Codec.Xlsx
import Control.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import Excel
import Pokemon.Types
    ( Move(mName, mTipe), MoveCategory(ATTACK), Pokemon(pName, pMoves) )
import Pokemon.Functions
    ( getBaseStat,
      getMoveCategory,
      getValue,
      maxSpeed,
      maxSpeedWithScarf,
      minStatAt,
      noInvestStatAt,
      neutralMaxStatAt,
      typeToColor
    )
import Data.List (sort)

-- | create an excel table with the pokemon sorted by speed. Uses the base speed stat to calculate speed rows for each mon
speedTable :: [Pokemon] -> ExcelTable
speedTable poks = table
  where
    headers' = map toBoldCellValue ["Name", "Base speed", "Min speed", "No invest speed", "Neutral max speed", "Max speed", "Max speed with scarf"]
    contents' = map pokemonSpeedRow poks
    table = ExcelTable headers' contents' HORIZONTAL

-- | List in order, name, min speed, no invest speed, max speed, max speed with scarf
pokemonSpeedRow :: Pokemon -> [CellValue]
pokemonSpeedRow pok = row
  where
    speed = getBaseStat "speed" pok
    pokName = pName pok
    row =
      [ CellText pokName,
        (CellDouble . fromIntegral . getValue) speed,
        (CellDouble . fromIntegral . minStatAt 100) speed,
        (CellDouble . fromIntegral . noInvestStatAt 100) speed,
        (CellDouble . fromIntegral . neutralMaxStatAt 100) speed,
        (CellDouble . fromIntegral . maxSpeed) pok,
        (CellDouble . fromIntegral . maxSpeedWithScarf) pok
      ]

pokemonMoveMap :: TableMode -> Pokemon -> Either String ExcelMap
pokemonMoveMap mode mon = do
  mvCts <- moveCategories mon
  let mp = ExcelMap (categoryMap M.empty mvCts) mode
  return mp

moveCategories :: Pokemon -> Either String [(T.Text, T.Text)]
moveCategories mon = do 
    maybeMvList <- pMoves mon
    let 
        sortedMoves = sort maybeMvList
        zipCategories = map (\mov -> (getMoveCategory mov, mov)) sortedMoves
        categories = map (\(tipe, mv) -> (moveTypeToName (tipe, mv), mName mv)) zipCategories
    return categories

eitherToMaybes :: [Either a b] -> [Maybe b]
eitherToMaybes [] = []
eitherToMaybes (Left _: eithers) = Nothing : eitherToMaybes eithers
eitherToMaybes (Right b: eithers) = Just b : eitherToMaybes eithers

moveTypeToName :: (MoveCategory, Move) -> T.Text
moveTypeToName (ATTACK, move) = T.pack . show $ mTipe move
moveTypeToName (tipe, move) = (T.pack . show) tipe

categoryMap :: M.Map CellValue [CellValue] -> [(T.Text, T.Text)] -> M.Map CellValue [CellValue]
categoryMap mp [] = mp
categoryMap mp ((key, val) : cs) =
  let mVal = M.lookup (toBoldAndColorCellValue key (typeToColor key)) mp
      newVal = maybe [CellText val] (CellText val :) mVal
      mp' = M.insert (toBoldAndColorCellValue key (typeToColor key)) newVal mp
   in categoryMap mp' cs

