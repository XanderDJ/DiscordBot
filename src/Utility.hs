module Utility where
    
import qualified Data.Text as T
import Data.Char (isDigit)

parseToken :: T.Text -> Maybe T.Text
parseToken t = if T.length digit == 0 then Nothing else Just digit
 where digit = T.takeWhile isDigit t