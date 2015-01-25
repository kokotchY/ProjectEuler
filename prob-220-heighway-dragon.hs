import Data.Char (isUpper)

d :: Int -> String
d 0 = "Fa"
d i = myReplace $ d (i-1)

myReplace :: String -> String
myReplace [] = []
myReplace ('a':xs) = "aRbFR" ++ myReplace xs
myReplace ('b':xs) = "LFaLb" ++ myReplace xs
myReplace (x:xs) = x:myReplace xs

data Direction = DUp
    | DLeft
    | DRight
    | DDown
    deriving (Show)

data Position = Position (Int, Int) Direction
    deriving (Show)

getPos :: Position -> String -> [(Bool,Position)]
getPos pos plan = getPos' pos (filter isUpper plan)
    where
        getPos' :: Position -> String -> [(Bool,Position)]
        getPos' pos ('F':xs) = let newPosition = moveForward pos in (True,newPosition):getPos newPosition xs
        getPos' pos ('L':xs) = let newPosition = turnLeft pos in (False,newPosition):getPos newPosition xs
        getPos' pos ('R':xs) = let newPosition = turnRight pos in (False,newPosition):getPos newPosition xs

moveForward :: Position -> Position
moveForward (Position (x,y) DUp) = (Position (x, y+1) DUp)
moveForward (Position (x,y) DLeft) = (Position (x-1, y) DLeft)
moveForward (Position (x,y) DRight) = (Position (x+1, y) DRight)
moveForward (Position (x,y) DDown) = (Position (x, y-1) DDown)

turnLeft :: Position -> Position
turnLeft (Position (x,y) DUp) = (Position (x,y) DLeft)
turnLeft (Position (x,y) DDown) = (Position (x,y) DRight)
turnLeft (Position (x,y) DRight) = (Position (x,y) DUp)
turnLeft (Position (x,y) DLeft) = (Position (x,y) DDown)

turnRight :: Position -> Position
turnRight (Position (x,y) DUp) = (Position (x,y) DRight)
turnRight (Position (x,y) DDown) = (Position (x,y) DLeft)
turnRight (Position (x,y) DRight) = (Position (x,y) DDown)
turnRight (Position (x,y) DLeft) = (Position (x,y) DUp)

startPosition :: Position
startPosition = Position (0,0) DUp

nbSteps :: Int
nbSteps = 10^12

dragonNb :: Int
dragonNb = 50

main :: IO ()
main = putStrLn $ show $ snd $ last $ take nbSteps  $ filter fst $ getPos startPosition $ d dragonNb
