
primes :: [Int]
primes = sieve [2..]
    where
        sieve (p:x) = p : sieve [ n | n <- x, n `mod` p > 0 ]

main :: IO ()
main = print result
    where
        result :: Int
        result = sum $ takeWhile (<2000000) $ filter isPrime [2..]


isPrime :: Int -> Bool
isPrime nb = not $ any ((==0) . mod nb) [2..(floor . sqrt . fromIntegral) nb]
