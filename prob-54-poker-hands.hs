data Cards = Number Int | Jack | Queen | King | Ace
    deriving (Show, Eq)

data Hand = HighCard Cards
    | OnePair Cards
    | TwoPair Cards Cards
    | ThreeKind Cards
    | Straigh [Cards]
    | Flush [Cards]
    | FullHouse Cards Cards
    | FourKind Cards
    | StraightFlush [Cards]
    | RoyalFlush [Cards]

instance Ord Cards where
    a `compare` b = compare (cardValue a) (cardValue b)
        where
            cardValue (Number nb) = nb
            cardValue Jack = 11
            cardValue Queen = 12
            cardValue King = 13
            cardValue Ace = 14
