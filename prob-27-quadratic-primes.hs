import Data.List (maximumBy)

quadratic :: Int -> Int -> Int -> Int
quadratic n a b = n*n + a*n + b

isPrime :: Int -> Bool
isPrime nb
    | nb < 0 = False
    | otherwise = not $ any ((==0) . mod nb) [2..(floor . sqrt . fromIntegral) nb]

testQuadratic :: Int -> Int -> (Int, Int, Int)
testQuadratic a b = (length $ takeWhile (\x -> isPrime (quadratic x a b)) [0..], a, b)

main :: IO ()
main = print $ a * b
    where
        (nb, a, b) = maximumBy (\(nb1,a1,b1) (nb2,a2,b2) -> compare nb1 nb2) [testQuadratic a b | a <- [-999..999], b <- [-999..999]]
