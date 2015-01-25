import Data.List (sort, nub)

testSolution :: Int -> Int
testSolution p = length $ nub $ map myConvert $ filter (\(a,b,c) -> a^2 + b^2 == c^2) [(a,b,c) | a <- [1..1000], b <- [1..1000], c <- [1..1000], a+b+c == p]

myConvert :: (Int, Int, Int) -> [Int]
myConvert (a,b,c) = sort [a,b,c]
