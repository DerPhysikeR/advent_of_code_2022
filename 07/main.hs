import Data.Map qualified as M
import Data.List (foldl')
import Data.Char (isDigit)

data FileTree = File {size :: Int} | Directory (M.Map String FileTree) deriving (Show)
type Path = [String]
type State = (Path, FileTree)

unionDirs :: FileTree -> FileTree -> FileTree
unionDirs (Directory m1) (Directory m2) = Directory (M.union m1 m2)

insertInFileTree :: Path -> FileTree -> FileTree -> FileTree
insertInFileTree [name] f@(File _) (Directory m) = Directory (M.insert name f m)
insertInFileTree [name] d@(Directory _) (Directory m) = Directory (M.insertWith unionDirs name d m)
insertInFileTree (p:ps) toInsert ft@(Directory m) = case M.lookup p m of
    Just ft -> Directory (M.insertWith unionDirs p (insertInFileTree ps toInsert ft) m)
    Nothing -> Directory (M.insert p (insertInFileTree ps toInsert ft) m)

sizeOf :: FileTree -> Int
sizeOf (File size) = size
sizeOf (Directory m) = sum $ map (\(_, ft) -> sizeOf ft) $ M.toList m

joinPath :: Path -> String
joinPath = concatMap ("/" ++)

showFiles :: Path -> FileTree -> String
showFiles p (File size) = joinPath p ++ " " ++ show size ++ "\n"
showFiles p (Directory m) = concatMap (\(name, ft) -> showFiles (p ++ [name]) ft) $ M.toList m

parseFile :: String -> (String, FileTree)
parseFile s = (tail $ dropWhile isDigit s, File $ read $ takeWhile isDigit s)

parseLine :: State -> String -> State
parseLine (p, ft) ('$':' ':'c':'d':' ':dir)
    | dir == "/" = ([], ft)
    | dir == ".." = (init p, ft)
    | otherwise = (p ++ [dir], ft)
parseLine s ('$':' ':'l':'s':_) = s
parseLine (p, ft) ('d':'i':'r':' ':dir) = (p, insertInFileTree (p ++ [dir]) (Directory M.empty) ft)
parseLine (p, ft) line = (p, insertInFileTree (p ++ [name]) file ft)
    where (name, file) = parseFile line

getAllDirectorySizes :: FileTree -> [Int]
getAllDirectorySizes (File _) = []
getAllDirectorySizes d@(Directory m) = sizeOf d : concatMap getAllDirectorySizes [ft | (_, ft) <- M.toList m]

main :: IO ()
main = do
    (_, filemap) <- foldl' parseLine ([], Directory M.empty) . lines <$> readFile "input.txt"
    let allDirectorySizes = getAllDirectorySizes filemap
    print $ sum $ filter (<= 100000) allDirectorySizes
    let necessarySpaceToDelete = 30000000 - (70000000 - sizeOf filemap)
    print $ minimum $ filter (> necessarySpaceToDelete) allDirectorySizes
