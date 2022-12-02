data RPS = Rock | Paper | Scissors deriving (Show, Eq)
data GameState = Win1 | Win2 | Draw deriving (Show, Eq)

play :: RPS -> RPS -> GameState
play Rock Rock = Draw
play Rock Paper = Win2
play Rock Scissors = Win1
play Paper Rock = Win1
play Paper Paper = Draw
play Paper Scissors = Win2
play Scissors Rock = Win2
play Scissors Paper = Win1
play Scissors Scissors = Draw

parseLetter :: Char -> RPS
parseLetter 'A' = Rock
parseLetter 'B' = Paper
parseLetter 'C' = Scissors
parseLetter 'X' = Rock
parseLetter 'Y' = Paper
parseLetter 'Z' = Scissors

parseLetters :: String -> (RPS, RPS)
parseLetters [p1, _, p2] = (parseLetter p1, parseLetter p2)

scoreRPS :: RPS -> Int
scoreRPS Rock = 1
scoreRPS Paper = 2
scoreRPS Scissors = 3

scoreGameState :: GameState -> Int
scoreGameState Win1 = 0
scoreGameState Draw = 3
scoreGameState Win2 = 6

score :: (RPS, RPS) -> Int
score (p1, p2) = scoreRPS p2 + scoreGameState (play p1 p2)

main = do
    strategyGuide <- map parseLetters . lines <$> readFile "input.txt"
    print $ sum $ map score strategyGuide
