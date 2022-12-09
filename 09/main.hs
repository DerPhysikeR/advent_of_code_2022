import Data.Set qualified as S
import Data.Foldable (Foldable(foldl'))

type Coords = (Int, Int)
data Direction = R | U | L | D deriving (Show, Read, Bounded, Enum)
data Move = Move Direction Int deriving (Show, Read)
type Rope = [Coords]

calcDistance :: Coords -> Coords -> Int
calcDistance c1@(x1, y1) c2@(x2, y2)
    | c1 == c2 = 0
    | x1 == x2 && isTouching c1 c2 = 1
    | y1 == y2 && isTouching c1 c2= 1
    | isTouching c1 c2 = 2
    | otherwise = 3

moves :: [Coords]
moves = [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (-1, 1), (-1, -1), (1, -1)]

move :: Coords -> Coords -> Coords
move (x, y) (dx, dy) = (x + dx, y + dy)

isTouching :: Coords -> Coords -> Bool
isTouching c1 c2 = c1 == c2 || (c1 `elem` map (move c2) moves)

toCoords :: [Move] -> [Coords]
toCoords [] = []
toCoords [Move d count] = replicate count (moves !! fromEnum d)
toCoords ((Move d count):ms) = replicate count (moves !! fromEnum d) ++ toCoords ms

moveTail :: Rope -> Rope
moveTail r@(h@(hx, hy):t@(tx, ty):_)
    | isTouching h t = r
    | otherwise = [h, newT]
        where allNewTs = map (move t) moves
              distances = map (calcDistance h) allNewTs
              minDistance = minimum distances
              newT = fst $ head $ filter (\(_, d) -> d == minDistance) (zip allNewTs distances)

moveRope :: Rope -> Coords -> Rope
moveRope (h:t:_) delta = moveTail [move h delta, t]

moveLongTail :: Rope -> Rope
moveLongTail [] = []
moveLongTail [x] = [x]
moveLongTail (h:t:rs) = h : moveLongTail (newTail:rs)
    where (_:newTail:_) = moveTail [h, t]

moveLongRope :: Rope -> Coords -> Rope
moveLongRope (h:rs) delta = moveLongTail (move h delta : rs)

main :: IO ()
main = do
    moves :: [Coords] <- toCoords . map (\line -> read ("Move " ++ line)) . lines <$> readFile "input.txt"
    let (finalRope, visitedPositions) = foldl' (\(rope, visitedPositions) m -> let newRope@(_:t:_) = moveRope rope m in (newRope, S.insert t visitedPositions)) ([(0, 0), (0, 0)], S.singleton (0, 0)) moves
    print $ length visitedPositions
    let (finalRope, visitedPositions) = foldl' (\(rope, visitedPositions) m -> let newRope = moveLongRope rope m in (newRope, S.insert (last newRope) visitedPositions)) (replicate 10 (0, 0), S.singleton (0, 0)) moves
    print $ length visitedPositions

