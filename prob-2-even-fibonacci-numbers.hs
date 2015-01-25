
fibs :: [Int]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = print result
    where
        result :: Int
        result = sum $ takeWhile (< 4000000) $ filter even fibs
