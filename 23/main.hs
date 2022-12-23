import qualified Data.Set as S

type Point = (Int, Int)
type Board = S.Set Point
data Direction = North | East | South | West deriving (Show, Eq)
type Directions = [Direction]
type GameState = (Board, Directions)

enumerate = zip [0..]

parseBoard :: String -> Board
parseBoard s = S.fromList $ concat [
    [(rowIdx, colIdx) | (colIdx, c) <- enumerate row, c == '#']
    | (rowIdx, row) <- enumerate $ lines s]

isDirFree :: Point -> Board -> Direction -> Bool
isDirFree (row, col) board North = not $ any (`S.member` board) [(row - 1, col - 1), (row - 1, col), (row - 1, col + 1)]
isDirFree (row, col) board East = not $ any (`S.member` board) [(row - 1, col + 1), (row, col + 1), (row + 1, col + 1)]
isDirFree (row, col) board South = not $ any (`S.member` board) [(row + 1, col - 1), (row + 1, col), (row + 1, col + 1)]
isDirFree (row, col) board West = not $ any (`S.member` board) [(row - 1, col - 1), (row, col - 1), (row + 1, col - 1)]

moveInDirection :: Point -> Direction -> Point
moveInDirection (row, col) North = (row - 1, col)
moveInDirection (row, col) East = (row, col + 1)
moveInDirection (row, col) South = (row + 1, col)
moveInDirection (row, col) West = (row, col - 1)

whereToMove :: Point -> Directions -> Board -> Point
whereToMove point@(row, col) dirs board
    | null freeDirs = point
    | length freeDirs == 4 = point
    | otherwise = moveInDirection point (head freeDirs)
    where freeDirs = filter (isDirFree point board) dirs

evolveGame :: GameState -> GameState
evolveGame (board, dirs) = (newBoard, newDirs)
    where newDirs = tail dirs ++ [head dirs]
          proposedMoves = map (\point -> (point, whereToMove point dirs board)) (S.toList board)
          duplicateTargets = snd $ foldr (\(_, to) (encountered, duplicates) ->
              if to `S.member` encountered
              then (encountered, S.insert to duplicates)
              else (S.insert to encountered, duplicates)
            ) (S.empty, S.empty) proposedMoves
          newBoard = S.fromList $ map (\(point, newPoint) ->
              if newPoint `S.member` duplicateTargets then point else newPoint
            ) proposedMoves

getBoundingCorners :: Board -> (Point, Point)
getBoundingCorners board = (minPoint, maxPoint)
    where (rows, cols) = foldr (\(row, col) (rows, cols) -> (row : rows, col : cols)) ([], []) (S.toList board)
          minPoint = (minimum rows, minimum cols)
          maxPoint = (maximum rows, maximum cols)

countFreeTiles :: Board -> Int
countFreeTiles board = length [(row, col) | row <- rows, col <- cols, (row, col) `S.notMember` board]
    where ((minRow, minCol), (maxRow, maxCol)) = getBoundingCorners board
          (rows, cols) = ([minRow..maxRow], [minCol..maxCol])

toStrBoard :: Board -> String
toStrBoard board = concat [[if (row, col) `S.member` board then '#' else '.' | col <- cols] ++ "\n" | row <- rows]
    where ((minRow, minCol), (maxRow, maxCol)) = getBoundingCorners board
          (rows, cols) = ([minRow..maxRow], [minCol..maxCol])

main :: IO ()
main = do
    board <- parseBoard <$> readFile "input.txt"
    let startGameState = (board, [North, South, West, East])
    let finalBoard = fst $ last $ take 11 $ iterate evolveGame startGameState
    print $ countFreeTiles finalBoard
