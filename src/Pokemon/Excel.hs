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
    ( Move(mName, mTipe), MoveType(ATTACK), Pokemon(pName, pMoves) )
import Pokemon.Functions
    ( getBaseStat,
      getMoveType,
      getValue,
      maxSpeed,
      maxSpeedWithScarf,
      minStatAt,
      noInvestStatAt )

-- | create an excel table with the pokemon sorted by speed. Uses the base speed stat to calculate speed rows for each mon
speedTable :: [Pokemon] -> ExcelTable
speedTable poks = table
  where
    headers' = map toBoldCellValue ["Name", "Base speed", "Min speed", "No invest speed", "Max speed", "Max speed with scarf"]
    contents' = map pokemonSpeedRow poks
    table = ExcelTable headers' contents' HORIZONTAL

-- | List in order, name, min speed, no invest speed, max speed, max speed with scarf
pokemonSpeedRow :: Pokemon -> [CellValue]
pokemonSpeedRow pok = row
  where
    speed = getBaseStat "speed" pok
    pokName = T.pack $ pName pok
    row =
      [ CellText pokName,
        (CellDouble . fromIntegral . getValue) speed,
        (CellDouble . fromIntegral . minStatAt 100) speed,
        (CellDouble . fromIntegral . noInvestStatAt 100) speed,
        (CellDouble . fromIntegral . maxSpeed) pok,
        (CellDouble . fromIntegral . maxSpeedWithScarf) pok
      ]

pokemonMoveMap :: TableMode -> Pokemon -> Either String ExcelMap
pokemonMoveMap mode mon = do
  mvCts <- moveCategories mon
  let mp = ExcelMap (categoryMap M.empty mvCts) mode
  return mp

moveCategories :: Pokemon -> Either String [(String, String)]
moveCategories mon = do 
    maybeMvList <- pMoves mon
    let zipCategories = map (\mov -> (getMoveType mov, mov)) maybeMvList
        categories = map (\(tipe, mv) -> (moveTypeToName (tipe, mv), mName mv)) zipCategories
    return categories

eitherToMaybes :: [Either a b] -> [Maybe b]
eitherToMaybes [] = []
eitherToMaybes (Left _: eithers) = Nothing : eitherToMaybes eithers
eitherToMaybes (Right b: eithers) = Just b : eitherToMaybes eithers

moveTypeToName :: (MoveType, Move) -> String
moveTypeToName (ATTACK, move) = show $ mTipe move
moveTypeToName (tipe, move) = show tipe

categoryMap :: M.Map CellValue [CellValue] -> [(String, String)] -> M.Map CellValue [CellValue]
categoryMap mp [] = mp
categoryMap mp ((key, val) : cs) =
  let mVal = M.lookup (toBoldCellValue key) mp
      newVal = maybe [sToCell val] (sToCell val :) mVal
      mp' = M.insert (toBoldCellValue key) newVal mp
   in categoryMap mp' cs

sToCell :: String -> CellValue
sToCell = CellText . T.pack
