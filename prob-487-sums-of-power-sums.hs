
f :: Int -> Int -> Int
f k n = sum $ map (\x -> x ^ k) [1..n]

s :: Int -> Int -> Int
s k n = sum $ map (f k) [1..n]
