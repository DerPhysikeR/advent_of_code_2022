import Data.Map qualified as M
import Data.List (foldl')
import Data.Char (isDigit)

data FileObject = File {name :: String, size :: Int} | Directory {name :: String} deriving (Show)
type Path = [String]
type State = (Path, M.Map Path [FileObject])

parseFile :: String -> FileObject
parseFile s = File (tail $ dropWhile isDigit s) (read $ takeWhile isDigit s)

parseLine :: State -> String -> State
parseLine (p, m) ('$':' ':'c':'d':' ':dir)
    | dir == "/" = (["/"], m)
    | dir == ".." = (init p, m)
    | otherwise = (p ++ [dir], m)
parseLine s ('$':' ':'l':'s':_) = s
parseLine (p, m) ('d':'i':'r':' ':dir) = (p, M.insertWith (++) p [Directory dir] m)
parseLine (p, m) line = (p, M.insertWith (++) p [parseFile line] m)

main :: IO ()
main = do
    (_, filemap) <- foldl' parseLine ([""], M.empty) . lines <$> readFile "test_input.txt"
    print filemap
