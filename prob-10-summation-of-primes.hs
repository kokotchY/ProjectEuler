
primes :: [Int]
primes = sieve [2..]
    where
        sieve (p:x) = p : sieve [ n | n <- x, n `mod` p > 0 ]

main :: IO ()
main = putStrLn $ show result
    where
        result :: Int
        result = sum $ takeWhile (<2000000) primes
