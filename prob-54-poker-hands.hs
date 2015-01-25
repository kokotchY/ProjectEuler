import Data.List (sort, group,sortBy,minimumBy)
import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import Control.Arrow ((&&&))

data Cards = Number Int Suits | Jack Suits | Queen Suits | King Suits | Ace Suits
    deriving (Show, Eq)

data Suits = Club
    | Spade
    | Hearth
    | Diamond
    deriving (Show, Eq)

data Hand = HighCard
    | OnePair
    | TwoPair
    | ThreeKind
    | Straight
    | Flush
    | FullHouse
    | FourKind
    | StraightFlush
    | RoyalFlush
    deriving (Show, Ord, Eq)

instance Ord Cards where
    a `compare` b = compare (getCardValue a) (getCardValue b)

doTest :: IO ()
doTest = mapM_ (print . getHand) cards
    where
        cards :: [[Cards]]
        cards = [[Number 2 Club, Number 3 Hearth, Number 4 Spade, Number 5 Spade, Number 6 Diamond],
                 [Number 2 Club, Number 4 Club, Number 6 Club, Number 7 Club, Number 10 Club],
                 [Jack Club, Number 4 Club, Jack Hearth, Jack Diamond, Number 4 Hearth],
                 [King Club, King Diamond, King Hearth, King Spade, Number 7 Hearth],
                 [Number 2 Club, Number 3 Club, Number 4 Club, Number 5 Club, Number 6 Club],
                 [Number 10 Club, Jack Club, Queen Club, King Club, Ace Club],
                 [Number 3 Club, Number 3 Hearth, Number 3 Diamond, Number 4 Diamond, King Hearth],
                 [Number 3 Club, Number 3 Hearth, Number 4 Diamond, Number 4 Club, King Hearth],
                 [Number 3 Club, Number 3 Hearth, Number 5 Diamond, Number 4 Club, King Hearth],
                 [Number 4 Club, Number 5 Hearth, Number 9 Diamond, King Club, Ace Hearth]
                ]

data Player =
    PlayerOne
    | PlayerTwo
    | Unknown
    | Unknown2
    deriving (Show, Eq)

main :: IO ()
main = do
    fileContent <- readFile "p054_poker.txt"
    let winners = map (getWinner . convertLineToCards) $ lines fileContent
    putStrLn $ "Winner player1: " ++ show (nbWin PlayerOne winners)
    putStrLn $ "Winner player2: " ++ show (nbWin PlayerTwo winners)
    putStrLn $ "Winner unknown: " ++ show (nbWin Unknown winners)
    putStrLn $ "Winner unknown2: " ++ show (nbWin Unknown2 winners)
    where
        nbWin :: Player -> [Player] -> Int
        nbWin player winners = length $ filter (== player) winners

getWinner :: ([Cards], [Cards]) -> Player
getWinner (c1, c2) =
    case compare handP1 handP2 of
        EQ -> compareValue handP1 c1 c2
        GT -> PlayerOne
        LT -> PlayerTwo
    where
        handP1 = getHand c1
        handP2 = getHand c2

compareValue :: Hand -> [Cards] -> [Cards] -> Player
compareValue hand c1 c2 =
    case hand of
        HighCard -> getHighest highestCardValue c1 c2
        OnePair -> getHighest highestPairValue c1 c2
        otherwise -> Unknown

highestCardValue :: [Cards] -> Int
highestCardValue = getCardValue . minimumBy (flip compare)

highestPairValue :: [Cards] -> Int
highestPairValue cards  = snd $ head $ filter ((==2) . fst) $ generalFrequency getCardValue cards

getHighest :: ([Cards] -> Int) -> [Cards] -> [Cards] -> Player
getHighest f c1 c2
    | v1 > v2 = PlayerOne
    | v1 < v2 = PlayerTwo
    | v1 == v2 = Unknown2
    where
        v1 = f c1
        v2 = f c2

convertLineToCards :: String -> ([Cards], [Cards])
convertLineToCards line = (take 5 cards, drop 5 cards)
    where
        cards :: [Cards]
        cards = map convertToCard $ splitOn " " line

convertCardsToHand :: ([Cards], [Cards]) -> (Hand, Hand)
convertCardsToHand (c1, c2) = (getHand c1, getHand c2)

convertToCards :: String -> [Cards]
convertToCards = map convertToCard . splitOn " "

convertToCard :: String -> Cards
convertToCard [v,s]
    | v `elem` map ((!! 0) . show) [2..9] = Number (digitToInt v) suits
    | v == 'T' = Number 10 suits
    | v == 'J' = Jack suits
    | v == 'Q' = Queen suits
    | v == 'K' = King suits
    | v == 'A' = Ace suits
    where
        suits = convertToSuits s

convertToSuits :: Char -> Suits
convertToSuits 'H' = Hearth
convertToSuits 'C' = Club
convertToSuits 'S' = Spade
convertToSuits 'D' = Diamond


getHand :: [Cards] -> Hand
getHand cards = getHand' $ sort cards
getHand' :: [Cards] -> Hand
getHand' cards
    | isRoyalFlush cards = RoyalFlush
    | isStraightFlush cards = StraightFlush
    | isXofAKind 4 cards = FourKind
    | isFullHouse cards = FullHouse
    | isFlush cards = Flush
    | isStraight cards = Straight
    | isXofAKind 3 cards = ThreeKind
    | isTwoPairs cards = TwoPair
    | isPair cards = OnePair
    | otherwise = HighCard

isPair :: [Cards] -> Bool
isPair cards = length (filter ((==2) . fst) $ generalFrequency getCardValue cards) == 1

isTwoPairs :: [Cards] -> Bool
isTwoPairs cards = length (filter ((==2) . fst) frequencies) == 2
    where
        frequencies :: [(Int, Int)]
        frequencies = generalFrequency getCardValue cards

isXofAKind :: Int -> [Cards] -> Bool
isXofAKind nb cards = length (filter ((== nb) . fst) $ generalFrequency getCardValue cards) == 1

isRoyalFlush :: [Cards] -> Bool
isRoyalFlush cards = sameSuits cards && map getCardValue cards == [10, 11, 12, 13, 14]

isFullHouse :: [Cards] -> Bool
isFullHouse cards = length frequencies == 2 && hasOccurence 3 frequencies && hasOccurence 2 frequencies
    where
        frequencies :: [(Int, Int)]
        frequencies = generalFrequency getCardValue cards

isFlush :: [Cards] -> Bool
isFlush = sameSuits

hasOccurence :: Int -> [(Int, Int)] -> Bool
hasOccurence value occurences =
    case lookup value occurences of
        Nothing -> False
        Just _ -> True

sameSuits :: [Cards] -> Bool
sameSuits cards = length (generalFrequency getSuits cards) == 1

isStraightFlush :: [Cards] -> Bool
isStraightFlush cards = sameSuits cards && isStraight cards

isStraight :: [Cards] -> Bool
isStraight cards = snd (foldl (\(previous, result) y -> if previous+1 == y then (y, result) else (y, False)) (head cards' - 1, True) cards')
    where
        cards' = map getCardValue cards

createListFromNb :: Suits -> [Int] -> [Cards]
createListFromNb suits = map (`Number` suits)

generalFrequency :: Eq a => (Cards -> a) -> [Cards] -> [(Int, a)]
generalFrequency f list = map (length &&& head) (group (map f (sort list)))

getSuits :: Cards -> Suits
getSuits (Number _ suits) = suits
getSuits (Jack suits) = suits
getSuits (Queen suits) = suits
getSuits (King suits) = suits
getSuits (Ace suits) = suits

getCardValue :: Cards -> Int
getCardValue (Number nb _) = nb
getCardValue (Jack _) = 11
getCardValue (Queen _) = 12
getCardValue (King _) = 13
getCardValue (Ace _) = 14
