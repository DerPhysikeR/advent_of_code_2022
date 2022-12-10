import Data.Foldable (Foldable(foldl'))
data Instruction = Noop | AddX Int
type Instructions = [Instruction]
type Tape = [Int -> Int]

cycles :: [Int]
cycles = [20, 60, 100, 140, 180, 220]

parseLine :: String -> Instruction
parseLine "noop" = Noop
parseLine ('a':'d':'d':'x':' ':rest) = AddX (read rest)

toTape :: Instructions -> Tape
toTape [] = []
toTape (Noop:xs) = id : toTape xs
toTape ((AddX v):xs) = id : (+v) : toTape xs

main :: IO ()
main = do
    instructions <- map parseLine . lines <$> readFile "input.txt"
    let tape = toTape instructions
    let results = reverse $ foldl' (\xs@(x:_) f -> f x : xs) [1] tape
    let indexedResult = zipWith (\i c -> (i, c, i * c)) [1..] results
    let result = filter (\(i, _, _) -> i `elem` cycles) indexedResult
    print $ sum $ map (\(_, _, x) -> x) result
