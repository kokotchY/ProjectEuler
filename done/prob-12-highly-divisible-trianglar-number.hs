tri :: Int -> Int
tri n = n*(n+1) `div` 2

divisors :: Int -> [Int]
divisors nb = concatMap (\x -> [x, nb `div` x]) $ filter ((==0) . (mod nb)) [1..upperBound]
    where
        upperBound :: Int
        upperBound = (floor . sqrt . fromIntegral) nb

prob :: Int -> Int
prob nbDivisors = maximum $ head $ filter ((> nbDivisors) . length) $ map (divisors . tri) [1..]

main :: IO ()
main = putStrLn $ show $ prob 500
