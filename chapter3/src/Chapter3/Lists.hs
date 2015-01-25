module Chapter3.Lists where

import Chapter3.ParamPoly (Client(..), getClientName)

product1 :: [Integer] -> Integer
product1 [] = 1
product1 (x:xs) = x * product1 xs

product2 :: [Integer] -> Integer
product2 = foldr (*) 1

{-
 - Prelude Chapter3.Lists Chapter3.ParamPoly> minimumClient [GovOrg {clientId = 1::Int, clientName = "NATO"}, Individual {clientId = 2::Int, person = Person {firstName = "A", lastName = "B"}}]
 - Just (Individual {clientId = 2, person = Person {firstName = "A", lastName = "B"}})
 -}
minimumClient :: [Client a] -> Maybe (Client a)
minimumClient [] = Nothing
minimumClient [x] = Just x
minimumClient (x:xs) = let Just minXs = minimumClient xs
                           nameX = getClientName x
                           nameXs = getClientName minXs
                       in if (length nameXs < length nameX) then Just minXs else Just x

minimumClient' :: [Client a] -> Maybe (Client a)
minimumClient' = foldl minClient Nothing
                 where minClient initial x = case initial of
                                             Nothing -> Just x
                                             Just i  -> if length (getClientName i) <= length (getClientName x)
                                                        then initial
                                                        else Just x

all1 :: [Bool] -> Bool
all1 [] = True
all1 (x:xs) = x && (all1 xs)

all2 :: [Bool] -> Bool
all2 = foldr (&&) True

minimumBy :: (Integer -> Integer) -> [Integer] -> Integer
minimumBy _ [] = error "Empty list"
minimumBy _ [x] = x
minimumBy f (x:xs) = let o = minimumBy f xs
                     in if (f x) <= (f o) then x else o

minimumBy' :: (Integer -> Integer) -> [Integer] -> Integer
minimumBy' _ [] = error "Empty list"
minimumBy' f xs = foldl1 (\a b -> if f a <= f b then a else b) xs
