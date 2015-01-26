list :: [(Int, Int)]
list = [(num, den) | num <- [10..99], den <- [10..99], num < den, num `mod` 10 /= 0 && den `mod` 10 /= 0]

testSolution :: (Int, Int) -> Bool
testSolution (num, den) = or [numD == denD && quotient1 == quotient numU denU,
    numD == denU && quotient1 == quotient numU denD,
    numU == denD && quotient1 == quotient numD denU,
    numU == denU && quotient1 == quotient numD denD]
    where
        quotient :: Int -> Int -> Double
        quotient a b = fromIntegral a / fromIntegral b
        quotient1 :: Double
        quotient1 = quotient num den
        (numD, numU)  = convertNumber num
        (denD, denU) = convertNumber den
        convertNumber :: Int -> (Int, Int)
        convertNumber nb = (nb `div` 10, nb `mod` 10)

main :: IO ()
main = print getSolution

getSolution :: Int
getSolution = snd $ calc $ multiplication $ filter testSolution list

multiplication :: [(Int, Int)] -> (Int, Int)
multiplication = foldl (\(a1, b1) (a2, b2) -> (a1*a2, b1*b2)) (1,1)

pgcd :: Int -> Int -> Int
pgcd a 0 = a
pgcd a b = pgcd b (a `mod` b)

calc :: (Int, Int) -> (Int, Int)
calc (a,b) = (a `div` v, b `div` v)
    where
        v = pgcd a b
