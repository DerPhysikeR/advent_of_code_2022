import qualified Data.Map as M
import Data.Maybe (isNothing)
import Control.Monad (forM)
import Data.Set qualified as S
import Debug.Trace (traceShow)
tr x = traceShow x x

type Point = (Int, Int)
data Direction = North | East | South | West deriving (Show, Eq)
data Tile = Wall | Storms [Direction] deriving (Show, Eq)
type Valley = M.Map Point Tile
type Valleys = [Valley]

type Node = (Point, Int)
type Predecessors = M.Map Node Node
type Heap = M.Map Node Int

move :: Point -> Direction -> Point
move (row, col) North = (row - 1, col)
move (row, col) East = (row, col + 1)
move (row, col) South = (row + 1, col)
move (row, col) West = (row, col - 1)

moveWrapAround :: Point -> Direction -> (Int, Int) -> Point
moveWrapAround p dir (width, height)
    | dir == North || dir == South = (saveMod row height, col) 
    | dir == East || dir == West = (row, saveMod col width)
    where (row, col) = move p dir
          saveMod x y = mod (x + y) y

distance :: Point -> Point -> Int
distance (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

getNeighbors :: Point -> [Point]
getNeighbors p = p : map (move p) [North, East, South, West]

getFreeNeighbors :: Point -> Valley -> [Point]
getFreeNeighbors point valley = filter (\p -> isNothing (M.lookup p valley)) (getNeighbors point)

evolve :: Valley -> Valley
evolve valley = foldr insertTiles M.empty (M.toList valley)
    where (width, height) = getWidthHeight valley
          insertTiles (p, Wall) v = M.insert p Wall v
          insertTiles (p, Storms []) v = v
          insertTiles (p, Storms (d:ds)) v = insertTiles (p, Storms ds) (M.insertWith unifyStorms newP newStorm v)
              where newP = moveWrapAround p d (width, height)
                    newStorm = Storms [d]
                    unifyStorms (Storms d1) (Storms d2) = Storms (d1 ++ d2)
                    unifyStorms Wall _ = error "Wall should never be unified with Storm"
                    unifyStorms _ Wall = error "Wall should never be unified with Storm"

getMinMax :: Valley -> (Point, Point)
getMinMax valley = ((-1, -1), (maxRow, maxCol))
    where maxRow = maximum $ map (fst . fst) (M.toList valley)
          maxCol = maximum $ map (snd . fst) (M.toList valley)

getWidthHeight :: Valley -> (Int, Int)
getWidthHeight valley = (maxCol, maxRow)
    where (_, (maxRow, maxCol)) = getMinMax valley

toCharTile :: Tile -> Char
toCharTile Wall = '#'
toCharTile (Storms xs@(_:_:_)) = last $ show $ length xs
toCharTile (Storms [North]) = '^'
toCharTile (Storms [East]) = '>'
toCharTile (Storms [South]) = 'v'
toCharTile (Storms [West]) = '<'
toCharTile s@(Storms []) = error ("No storms should be `Free`: " ++ show s)

toStrValley :: Valley -> String
toStrValley valley = concat [[maybe '.' toCharTile (M.lookup (r, c) valley)
    | c <- cols] ++ "\n" | r <- rows]
    where ((minRow, minCol), (maxRow, maxCol)) = getMinMax valley
          rows = [minRow..maxRow]
          cols = [minCol..maxCol]

parseChar :: Char -> Tile
parseChar '#' = Wall
parseChar '>' = Storms [East]
parseChar '^' = Storms [North]
parseChar '<' = Storms [West]
parseChar 'v' = Storms [South]

parseInput :: String -> Valley
parseInput s = M.fromList $ wallBehindStartingPoint : concat [[((rowIdx, colIdx), parseChar c) | (colIdx, c) <- enum row, c /= '.'] | (rowIdx, row) <- enum lns]
    where enum = zip [-1..]
          lns = lines s
          wallBehindStartingPoint = ((-2, 0), Wall)

getMinHeapEntry :: Heap -> (Heap, Node)
getMinHeapEntry heap = (M.delete minKey heap, minKey)
    where heapList = M.toList heap
          (minKey, _) = foldr (\(n, p) acc@(node, priority) -> if p < priority then (n, p) else acc) (head heapList) (tail heapList)

type Checked = S.Set Node
type SearchState = (Checked, Predecessors, Heap)

reconstructPath :: Node -> Predecessors -> [Node]
reconstructPath node predecessors = case M.lookup node predecessors of
    Nothing -> [node]
    Just pred -> if node == pred then error "cycle in predecessors" else reconstructPath pred predecessors ++ [node]

aStar :: Valleys -> Point -> SearchState -> [Node]
aStar valleys goal ss@(checked, predecessors, heap)
    | M.null heap = error "no path found"
    | goal `elem` neighboringPoints = reconstructPath node predecessors ++ [(goal, 0)]
    | otherwise = aStar valleys goal (newChecked, newPredecessors, newHeap)
    where (takenFromHeap, node@(point, time)) = getMinHeapEntry heap
          newChecked = S.insert node checked
          newT = time + 1
          neighboringPoints = getFreeNeighbors point (valleys !! newT)
          neighbors = filter (`S.notMember` newChecked) $ zip neighboringPoints (repeat newT)
          (newPredecessors, newHeap) = foldr (
              \n@(p, t) (pred, hp) -> case M.lookup n hp of
                    Nothing -> inserted
                        where newPrio = newT + distance p goal
                              inserted = (M.insert n node pred, M.insert n newPrio hp)
                    Just prio -> if prio < newPrio then (pred, hp) else inserted
                        where newPrio = newT + distance p goal
                              inserted = (M.insert n node pred, M.insert n newPrio hp)
              ) (predecessors, takenFromHeap) neighbors

findPath :: Valley -> [Point]
findPath valley = map fst nodePath
    where valleys = iterate evolve valley
          (_, (row, col)) = getMinMax valley
          endPoint = (row, col - 1)
          startPoint = (-1, 0)
          heap = M.singleton (startPoint, 0) 0
          nodePath = aStar valleys endPoint (S.empty, M.empty, heap)

main :: IO ()
main = do
    valley <- parseInput <$> readFile "input.txt"
    let path = findPath valley
    print $ length path - 1
