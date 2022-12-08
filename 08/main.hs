import Data.Char (digitToInt)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.List (foldl')

type Grid = [[Int]]
type Coords = (Int, Int)
type TreeMap = M.Map Coords Int
type Tree = (Coords, Int)
type Trees = [Tree]

addCoords :: Coords -> Coords -> Coords
addCoords (row1, col1) (row2, col2) = (row1 + row2, col1 + col2)

parseInput :: String -> Grid
parseInput = map (map digitToInt) . lines

enumerate = zip [0..]

gridToTrees :: Grid -> TreeMap
gridToTrees grid = M.fromList . concat $ [[((rowIdx, colIdx), tree) | (colIdx, tree) <- enumerate row] | (rowIdx, row) <- enumerate grid]

right = (0, 1)
up = (-1, 0)
left = (0, -1)
down = (1, 0)

lookInto :: Coords -> Coords -> TreeMap -> Trees
lookInto position direction treemap = case M.lookup position treemap of
    Nothing -> []
    Just height -> (position, height) : lookInto (addCoords position direction) direction treemap

filterVisible :: Trees -> Trees
filterVisible trees = snd $ foldl' (\(maxHeight, trees) tree@(c, h) -> if h > maxHeight then (h, tree : trees) else (maxHeight, trees)) (-1, []) trees

lookIntoForrest :: Coords -> Coords -> TreeMap -> Trees
lookIntoForrest position direction treemap = filterVisible $ lookInto position direction treemap

lookIntoForrestFromAllDirections :: Coords -> TreeMap -> S.Set Tree
lookIntoForrestFromAllDirections maxrc treemap = mconcat $ map S.fromList [fromLeft, fromTop, fromRight, fromBottom]
    where minrc = (0, 0)
          leftColumn = lookInto minrc down treemap
          topRow = lookInto minrc right treemap
          bottomRow = lookInto maxrc left treemap
          rightColumn = lookInto maxrc up treemap
          fromLeft = concat [lookIntoForrest c right treemap | (c, _) <- leftColumn]
          fromTop = concat [lookIntoForrest c down treemap | (c, _) <- topRow]
          fromRight = concat [lookIntoForrest c left treemap | (c, _) <- rightColumn]
          fromBottom = concat [lookIntoForrest c up treemap | (c, _) <- bottomRow]

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive f [] = []
takeWhileInclusive f [x] = [x]
takeWhileInclusive f (x:xs) = if f x then x : takeWhileInclusive f xs else [x]

calcScenicScore :: TreeMap -> Tree -> Int
calcScenicScore treemap tree@(coords, height) = product viewingDistances
    where viewingDistance direction = takeWhileInclusive (\tree@(c, h) -> h < height) (tail $ lookInto coords direction treemap)
          viewingDistances = map (length . viewingDistance) [right, up, left, down]

main :: IO ()
main = do
    grid <- parseInput <$> readFile "input.txt"
    let treemap = gridToTrees grid
    let maxrc = (subtract 1 $ length grid, subtract 1 $ length $ head grid)
    print $ length $ lookIntoForrestFromAllDirections maxrc treemap
    print $ maximum $ map (calcScenicScore treemap) (M.toList treemap)
