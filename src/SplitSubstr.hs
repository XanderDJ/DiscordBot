-- stackoverflow.com/questions/49228467

module SplitSubstr where

import Data.List (elemIndex, isPrefixOf, unfoldr)
import Data.List.Split (splitWhen) -- Package `split`.
import Data.Maybe (catMaybes, isNothing)

-- | Split a (possibly infinite) string at the occurrences of any of the given delimiters.
--
-- λ take 10 $ splitOnSubstrs ["||", "***"] "la||la***fa"
-- ["la","la","fa"]
--
-- λ take 10 $ splitOnSubstrs ["||", "***"] (cycle "la||la***fa||")
-- ["la","la","fa","la","la","fa","la","la","fa","la"]
splitOnSubstrs :: [String] -> String -> [String]
splitOnSubstrs delims =
  fmap catMaybes -- At this point, there will be only `Just` elements left.
    . splitWhen isNothing -- Now we may split at nothings.
    . unfoldr f -- Replace the occurences of delimiters with a `Nothing`.
  where
    f [] = Nothing
    f body@(x : xs) = case elemIndex True $ (`isPrefixOf` body) <$> delims of
      Just index -> return (Nothing, drop (length $ delims !! index) body)
      Nothing -> return (Just x, xs)