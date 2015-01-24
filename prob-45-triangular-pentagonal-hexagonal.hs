import Data.List

triangle :: Int -> Int
triangle n = n * (n+1) `div` 2

pentagonal :: Int -> Int
pentagonal n = n * (3*n - 1) `div` 2

hexagonal :: Int -> Int
hexagonal n = n * (2*n - 1)

triangles :: [Int]
triangles = map triangle [1..]

pentagonals :: [Int]
pentagonals = map pentagonal [1..]

hexagonals :: [Int]
hexagonals = map hexagonal [1..]
