{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Foldable (Foldable(foldl'))

type Point = (Int, Int)
data Rotation = RL | RR deriving (Show)
data Direction = DR | DU | DL | DD deriving (Show, Eq, Ord, Bounded, Enum)
data Instruction = Walk | Rotate Rotation deriving (Show)
type Instructions = [Instruction]
data Field = Tile | Wall deriving (Show)
type Maze = M.Map Point Field
type Position = (Point, Direction)

class (Ord a, Bounded a, Enum a) => CyclicEnum a where
    csucc :: a -> a
    cpred :: a -> a

instance CyclicEnum Direction where
    csucc x = if x == maxBound then minBound else succ x
    cpred x = if x == minBound then maxBound else pred x

enumerate = zip [1..]

parseMaze :: T.Text -> Maze
parseMaze t = M.fromList $ concat [
        [let p = (rowIdx, colIdx) in if c == '.' then (p, Tile) else (p, Wall)
        | (colIdx, c) <- enumerate row, c `elem` T.unpack ".#"]
    | (rowIdx, row) <- enumerate lines]
    where lines = map T.unpack $ T.lines t

parseInstructions :: String -> Instructions
parseInstructions [] = []
parseInstructions ('\n' : _) = []
parseInstructions ('R' : rest) = Rotate RR : parseInstructions rest
parseInstructions ('L' : rest) = Rotate RL : parseInstructions rest
parseInstructions s@(x:xs)
    | isDigit x = replicate num Walk ++ parseInstructions rest
    | otherwise = error ("can't parse '" ++ [x] ++ "'")
  where
    (num, rest) = (read $ takeWhile isDigit s, dropWhile isDigit s)

parseInput :: T.Text -> (Maze, Instructions)
parseInput t = (parseMaze mazeText, parseInstructions (T.unpack instructionText))
  where
    (mazeText : instructionText : _) = T.splitOn "\n\n" t

calcPassword :: Position -> Int
calcPassword ((row, col), DR) = 1000 * row + 4 * col + 0
calcPassword ((row, col), DD) = 1000 * row + 4 * col + 1
calcPassword ((row, col), DL) = 1000 * row + 4 * col + 2
calcPassword ((row, col), DU) = 1000 * row + 4 * col + 3

getStartingPoint :: Maze -> Point
getStartingPoint maze = case M.lookup startingPoint maze of
    Nothing -> error "starting point not found"
    Just _ -> startingPoint
    where minRow = minimum [row | ((row, _), _) <- M.toList maze]
          minCol = minimum [col | ((row, col), _) <- M.toList maze, row == minRow]
          startingPoint = (minRow, minCol)

next :: Point -> Direction -> Point
next (row, col) DR = (row, col + 1)
next (row, col) DU = (row - 1, col)
next (row, col) DL = (row, col - 1)
next (row, col) DD = (row + 1, col)

getWrapAroundPosition :: Maze -> Position -> Position
getWrapAroundPosition maze ((row, _), DR) = ((row, minimum $ [c | ((r, c), _) <- M.toList maze, r == row]), DR)
getWrapAroundPosition maze ((_, col), DU) = ((maximum $ [r | ((r, c), _) <- M.toList maze, c == col], col), DU)
getWrapAroundPosition maze ((row, _), DL) = ((row, maximum $ [c | ((r, c), _) <- M.toList maze, r == row]), DL)
getWrapAroundPosition maze ((_, col), DD) = ((minimum $ [r | ((r, c), _) <- M.toList maze, c == col], col), DD)

getNextPosition :: Maze -> Position -> Position
getNextPosition maze position@(point, dir)
    | M.member nextPoint maze = (nextPoint, dir)
    | otherwise = getWrapAroundPosition maze position
    where nextPoint = next point dir

walk :: Maze -> Position -> Instruction -> Position
walk maze (point, dir) (Rotate RL) = (point, csucc dir)
walk maze (point, dir) (Rotate RR) = (point, cpred dir)
walk maze pos Walk = case M.lookup nextPoint maze of
    Nothing -> error $ "next point doesn't exist: " ++ show pos
    Just Tile -> nextPosition
    Just Wall -> pos
    where nextPosition@(nextPoint, nextDir) = getNextPosition maze pos

getWrapAroundPosition2 :: Maze -> Position -> Position
getWrapAroundPosition2 maze ((row, col), DR)
    | row <= 50 = ((151 - row, 100), DL)
    | row > 50 && row <= 100 = ((50, row + 50), DU)
    | row > 100 && row <= 150 = ((151 - row, 150), DL)
    | row > 150 = ((150, row - 100), DU)
getWrapAroundPosition2 maze ((row, col), DU)
    | col <= 50 = ((50 + col, 51), DR)
    | col > 50 && col <= 100 = ((100 + col, 1), DR)
    | col > 100 = ((200, col - 100), DU)
getWrapAroundPosition2 maze ((row, col), DL)
    | row <= 50 = ((151 - row, 1), DR)
    | row > 50 && row <= 100 = ((101, row - 50), DD)
    | row > 100 && row <= 150 = ((151 - row, 51), DR)
    | row > 150 = ((1, row - 100), DD)
getWrapAroundPosition2 maze ((row, col), DD)
    | col <= 50 = ((1, col + 100), DD)
    | col > 50 && col <= 100 = ((col + 100, 50), DL)
    | col > 100 = ((col - 50, 100), DL)

getNextPosition2 :: Maze -> Position -> Position
getNextPosition2 maze position@(point, dir)
    | M.member nextPoint maze = (nextPoint, dir)
    | otherwise = getWrapAroundPosition2 maze position
    where nextPoint = next point dir

walk2 :: Maze -> Position -> Instruction -> Position
walk2 maze (point, dir) (Rotate RL) = (point, csucc dir)
walk2 maze (point, dir) (Rotate RR) = (point, cpred dir)
walk2 maze pos Walk = case M.lookup nextPoint maze of
    Nothing -> error $ "next point doesn't exist: " ++ show pos
    Just Tile -> nextPosition
    Just Wall -> pos
    where nextPosition@(nextPoint, nextDir) = getNextPosition2 maze pos

main = do
  (maze, instructions) <- parseInput <$> TIO.readFile "input.txt"
  print $ calcPassword $ foldl' (walk maze) (getStartingPoint maze, DR) instructions
  print $ calcPassword $ foldl' (walk2 maze) (getStartingPoint maze, DR) instructions
