primes :: [Integer]
primes = nextList [2 ..]
         where nextList (x:xs) = x : (nextList $ filter (\i -> i `rem` x /= 0) xs)
