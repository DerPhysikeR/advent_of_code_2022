import Data.Set qualified as S
import Data.Foldable (Foldable(foldl'))

type Coords = (Int, Int)
data Direction = R | U | L | D deriving (Show, Read, Bounded, Enum)
data Move = Move Direction Int deriving (Show, Read)
data Rope = Rope {rhead :: Coords, rtail :: Coords} deriving (Show)

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
moveTail r@(Rope h@(hx, hy) t@(tx, ty))
    | isTouching h t = r
    | otherwise = Rope h newT
        where allNewTs = map (move t) moves
              distances = map (calcDistance h) allNewTs
              minDistance = minimum distances
              newT = fst $ head $ filter (\(_, d) -> d == minDistance) (zip allNewTs distances)

moveRope :: Rope -> Coords -> Rope
moveRope (Rope h t) delta = moveTail (Rope (move h delta) t)

main :: IO ()
main = do
    moves :: [Coords] <- toCoords . map (\line -> read ("Move " ++ line)) . lines <$> readFile "input.txt"
    let (finalRope, visitedPositions) = foldl' (\(rope, visitedPositions) m -> let newRope@(Rope _ t) = moveRope rope m in (newRope, S.insert t visitedPositions)) (Rope (0, 0) (0, 0), S.singleton (0, 0)) moves
    print $ length visitedPositions

