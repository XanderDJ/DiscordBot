module Excel (ExcelTable (..), ExcelMap (..), TableContent, TableMode (..), insertTable, insertMap, emptyXlsx, emptySheet, nextPoint, Size (..), toBoldCellValue) where

import Codec.Xlsx
    ( cellValueAt,
      runPropertiesBold,
      def,
      Worksheet,
      Xlsx,
      CellValue(CellRich),
      RichTextRun(RichTextRun) )
import Control.Lens (Field1 (_1), Field2 (_2), (%~), (&), (?~))
import qualified Data.Map as M
import qualified Data.Text as T

-- | Table with Excel data.
--  Mode specifies the direction of the table. Headers and contents as columns (VERTICAL) or rows (HORIZONTAL).
--  contents contain the columns/rows of the table
data ExcelTable = ExcelTable
  { eTHeaders :: [CellValue],
    eTContents :: [TableContent],
    eTMode :: TableMode
  }

-- | Data structure that allows for variable lengths of data in columns/row.
-- The keys of the map get used as headers and the values as the following row
data ExcelMap = ExcelMap
  { eMMap :: M.Map CellValue [CellValue],
    eMMode :: TableMode
  }
  deriving (Show)

data TableMode = HORIZONTAL | VERTICAL deriving (Show, Eq)

type TableContent = [CellValue]


insertTable :: (Int, Int) -> ExcelTable -> Worksheet -> Worksheet
insertTable pos table ws = finalWs
  where
    insertMethod = insertContent (eTMode table)
    next = nextPoint (reverseMode $ eTMode table)
    ws' = insertMethod pos (eTHeaders table) ws
    nextPos = next pos
    finalWs = foldrIndexed ws' nextPos next insertMethod (eTContents table)

insertMap :: (Int, Int) -> ExcelMap -> Worksheet -> Worksheet
insertMap startPos mp ws = finalWs
  where
    mapMode = eMMode mp
    map' = eMMap mp
    ((finalWs, mode, pos), map'') = M.mapAccumWithKey insertKeyValue (ws, mapMode, startPos) map'

-- | a = start value type.
-- b = index type.
-- c = list of other values.
-- (b -> b) = next index function.
-- (b -> c -> a -> a) = function that takes the index, a list of other values and a start value to produce another value.
-- This function will fold over a list of cs to produce an a that takes an index like a table of contents to insert cs into.
foldrIndexed :: a -> b -> (b -> b) -> (b -> c -> a -> a) -> [c] -> a
foldrIndexed startValue startIndex nextIndexF f (value : values) = let nextValue = f startIndex value startValue in foldrIndexed nextValue (nextIndexF startIndex) nextIndexF f values
foldrIndexed startValue _ _ _ [] = startValue

nextPoint :: TableMode -> (Int, Int) -> (Int, Int)
nextPoint HORIZONTAL = _2 %~ (+ 1)
nextPoint VERTICAL = _1 %~ (+ 1)

reverseMode :: TableMode -> TableMode
reverseMode HORIZONTAL = VERTICAL
reverseMode VERTICAL = HORIZONTAL

insertContent :: TableMode -> (Int, Int) -> [CellValue] -> Worksheet -> Worksheet
insertContent HORIZONTAL = insertRow
insertContent VERTICAL = insertCol

insertRow :: (Int, Int) -> [CellValue] -> Worksheet -> Worksheet
insertRow (row, col) (cv : cvs) ws = ws & cellValueAt (row, col) ?~ cv & insertRow (row, col + 1) cvs
insertRow _ [] ws = ws

insertCol :: (Int, Int) -> [CellValue] -> Worksheet -> Worksheet
insertCol (row, col) (cv : cvs) ws = ws & cellValueAt (row, col) ?~ cv & insertCol (row + 1, col) cvs
insertCol _ [] ws = ws

insertKeyValue :: (Worksheet, TableMode, (Int, Int)) -> CellValue -> [CellValue] -> ((Worksheet, TableMode, (Int, Int)), [CellValue])
insertKeyValue (ws, m, pos) header contents' = ((filledWs, m, nextPos), contents')
  where
    wsWithHeader = ws & cellValueAt pos ?~ header
    next = nextPoint (reverseMode m) pos
    nextPos = nextPoint m pos
    filledWs = insertContent (reverseMode m) next contents' wsWithHeader

emptyXlsx :: Xlsx
emptyXlsx = def

emptySheet :: Worksheet
emptySheet = def

class Size a where
  width :: a -> Int
  size :: a -> Int

instance Size ExcelTable where
  size table =
    let rows = length (eTContents table)
     in if eTMode table == HORIZONTAL then rows + 1 else width table
  width table = if eTMode table == HORIZONTAL then length (eTHeaders table) else size table

instance Size ExcelMap where
  width map = if eMMode map == HORIZONTAL then M.size $ eMMap map else size map
  size map = if eMMode map == HORIZONTAL then longestValLength (eMMap map) + 1 else width map

longestValLength :: Foldable t => M.Map a (t b) -> Int
longestValLength map =
  let f foldable currentMax = if length foldable > currentMax then length foldable else currentMax
   in M.foldr f 0 map

toBoldCellValue :: String -> CellValue
toBoldCellValue txt = 
  let
    runProperties = def
    boldProperty = runProperties & runPropertiesBold ?~ True
    richRun = RichTextRun (Just boldProperty) (T.pack txt)
  in
    CellRich [richRun]