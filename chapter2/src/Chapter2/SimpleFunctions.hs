module Chapter2.SimpleFunctions where

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 = if null lst1 {- проверка на пустоту -}
    then lst2                -- базовый случай
    else (head lst1) : (tail lst1 +++ lst2)

reverse2 :: [a] -> [a]
reverse2 lst = if null lst
    then []
    else reverse2 (tail lst) +++ [head lst]

maxmin :: (Ord a) => [a] -> (a, a)
maxmin list = let h = head list
              in if null (tail list)
                 then (h, h)
                 else ( if h > t_max then h else t_max
                      , if h < t_min then h else t_min )
                      where t = maxmin (tail list)
                            t_max = fst t
                            t_min = snd t

fibonacci :: Integer -> Maybe Integer
fibonacci n | n < 0     = Nothing
fibonacci 0             = Just 0
fibonacci 1             = Just 1
fibonacci n | otherwise = let (Just f1, Just f2) = (fibonacci (n - 1), fibonacci (n - 2))
                          in Just (f1 + f2)

fibonacciList :: [Integer]
fibonacciList = let fib f1 f2 = f1 + f2 : fib f2 (f1 + f2)
               in 0 : 1 : fib 0 1

fibonacci' :: Integer -> Maybe Integer
fibonacci' n
    | n < 0     = Nothing
    | otherwise = Just . last . take' (n + 1) $ fibonacciList

take' :: Integer -> [a] -> [a]
take' _ [] = []
take' n _ | n <= 0 = []
take' n (x:xs) = x : take' (n - 1) xs

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x, y):xs) = (x:as, y:bs)
                     where (as, bs) = unzip' xs
