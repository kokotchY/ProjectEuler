getPrime :: [(Int, Bool)] -> Int -> [(Int, Bool)]
getPrime _ 1 = [(1, False)]
getPrime primeList x =
    case lookup x primeList of
        Nothing -> (x,length divisors == 0) : primeList
                where
                    half :: Int
                    half = x `div` 2
                    divisors :: [Int]
                    divisors = filter (\y -> x `mod` y == 0) [2..half]
        Just result -> primeList

myAll :: [Bool] -> Bool
myAll [] = True
myAll (True:xs) = myAll xs
myAll (False:_) = False

allNumbers :: Int -> [Int]
allNumbers x = fromLeftToRight x' ++ fromRightToLeft x'
    where
        fromLeftToRight :: [Int] -> [Int]
        fromLeftToRight [] = []
        fromLeftToRight list = [getNb list] ++ fromLeftToRight (tail list)
        fromRightToLeft :: [Int] -> [Int]
        fromRightToLeft [] = []
        fromRightToLeft list = [getNb list] ++ fromRightToLeft (init list)
        x' = transform x

transform :: Int -> [Int]
transform x
    | x < 9 = [x]
    | otherwise = transform remain ++ [last]
    where
        last = x - remain*10
        remain = x `div` 10

getNb :: [Int] -> Int
getNb = foldl (\x y -> x*10+y) 0

checkNumber :: [(Int, Bool)] -> Int -> Bool
checkNumber = undefined
checkNumber primeList list = length notDivisor == 0
    where
        notDivisor :: [Int]
        notDivisor = filter (not . isPrime primeList) $ allNumbers list
