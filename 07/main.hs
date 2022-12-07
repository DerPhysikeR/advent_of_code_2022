import Data.Map qualified as M
import Data.List (foldl')

type Path = [String]
type State = (Path, M.Map Path [String])

parseLine :: State -> String -> State
parseLine (p, m) ('$':' ':'c':'d':' ':dir)
    | dir == "/" = (["/"], m)
    | dir == ".." = (init p, m)
    | otherwise = (p ++ [dir], m)
parseLine s ('$':' ':'l':'s':_) = s
parseLine (p, m) line = (p, M.insertWith (++) p [line] m)

main :: IO ()
main = do
    input <- foldl' parseLine ([""], M.empty) . lines <$> readFile "test_input.txt"
    print input
