module Data.List.Utility where


hasAny :: Eq a => [a] -> [a] -> Bool
hasAny [] _ = False
hasAny _ [] = False
hasAny (x:xs) search = (x `elem` search) || hasAny xs search