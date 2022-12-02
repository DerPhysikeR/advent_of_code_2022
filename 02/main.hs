data RPS = Rock | Paper | Scissors deriving (Show, Eq, Bounded, Enum)
data GameState = Lose | Draw | Win deriving (Show, Eq)

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred x
        | x == minBound = maxBound
        | otherwise = pred x
    csucc :: a -> a
    csucc x
        | x == maxBound = minBound
        | otherwise = succ x

instance CyclicEnum RPS

play :: RPS -> RPS -> GameState
play p1 p2
    | csucc p1 == p2 = Win
    | cpred p1 == p2 = Lose
    | otherwise = Draw

whatToPlay :: RPS -> GameState -> RPS
whatToPlay p1 Lose = cpred p1
whatToPlay p1 Draw = p1
whatToPlay p1 Win = csucc p1

parseLetter :: Char -> RPS
parseLetter 'A' = Rock
parseLetter 'B' = Paper
parseLetter 'C' = Scissors
parseLetter 'X' = Rock
parseLetter 'Y' = Paper
parseLetter 'Z' = Scissors

parseLetter2 :: Char -> GameState
parseLetter2 'X' = Lose
parseLetter2 'Y' = Draw
parseLetter2 'Z' = Win

parseLetters :: String -> (RPS, RPS)
parseLetters [p1, _, p2] = (parseLetter p1, parseLetter p2)

parseLetters2 :: String -> (RPS, GameState)
parseLetters2 [p1, _, p2] = (parseLetter p1, parseLetter2 p2)

scoreRPS :: RPS -> Int
scoreRPS Rock = 1
scoreRPS Paper = 2
scoreRPS Scissors = 3

scoreGameState :: GameState -> Int
scoreGameState Lose = 0
scoreGameState Draw = 3
scoreGameState Win = 6

score :: (RPS, RPS) -> Int
score (p1, p2) = scoreRPS p2 + scoreGameState (play p1 p2)

main = do
    content <- lines <$> readFile "input.txt"
    let strategyGuide = map parseLetters content
    print $ sum $ map score strategyGuide
    let strategyGuide = map parseLetters2 content
    print $ sum $ map (score . (\(p, g) -> (p, whatToPlay p g))) strategyGuide
