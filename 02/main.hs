data RPS = Rock | Paper | Scissors deriving (Show, Eq)
data GameState = Lose | Win | Draw deriving (Show, Eq)

play :: RPS -> RPS -> GameState
play Rock Rock = Draw
play Rock Paper = Win
play Rock Scissors = Lose
play Paper Rock = Lose
play Paper Paper = Draw
play Paper Scissors = Win
play Scissors Rock = Win
play Scissors Paper = Lose
play Scissors Scissors = Draw

whatToPlay :: RPS -> GameState -> RPS
whatToPlay Rock Lose = Scissors
whatToPlay Rock Win = Paper
whatToPlay Paper Lose = Rock
whatToPlay Paper Win = Scissors
whatToPlay Scissors Lose = Paper
whatToPlay Scissors Win = Rock
whatToPlay p1 Draw = p1

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
