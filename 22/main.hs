{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Foldable (Foldable(foldl'))
import Debug.Trace (traceShow)
tr x = traceShow x x

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

parseInstructions :: String -> Instructions -> (String, Instructions)
parseInstructions [] instr = ([], instr)
parseInstructions ('R' : rest) instr = parseInstructions rest (instr ++ [Rotate RR])
parseInstructions ('L' : rest) instr = parseInstructions rest (instr ++ [Rotate RL])
parseInstructions ('\n' : rest) instr = (rest, instr)
parseInstructions s@(x:xs) instr
    | isDigit x = parseInstructions rest (instr ++ replicate num Walk)
    | otherwise = error ("can't parse '" ++ [x] ++ "'")
  where
    (num, rest) = (read $ takeWhile isDigit s, dropWhile isDigit s)

parseInput :: T.Text -> (Maze, Instructions)
parseInput t = (parseMaze mazeText, snd $ parseInstructions (T.unpack instructionText) [])
  where
    (mazeText : instructionText : _) = T.splitOn "\n\n" t

calcPassword :: Position -> Int
calcPassword ((row, col), DR) = 1000 * row + 4 * col + 0
calcPassword ((row, col), DD) = 1000 * row + 4 * col + 1
calcPassword ((row, col), DL) = 1000 * row + 4 * col + 2
calcPassword ((row, col), DU) = 1000 * row + 4 * col + 3

next :: Point -> Direction -> Point
next (row, col) DR = (row, col + 1)
next (row, col) DU = (row - 1, col)
next (row, col) DL = (row, col - 1)
next (row, col) DD = (row + 1, col)

getWrapAroundPoint :: Maze -> Position -> Point
getWrapAroundPoint maze ((row, _), DR) = (row, minimum $ [c | ((r, c), _) <- M.toList maze, r == row])
getWrapAroundPoint maze ((_, col), DU) = (maximum $ [r | ((r, c), _) <- M.toList maze, c == col], col)
getWrapAroundPoint maze ((row, _), DL) = (row, maximum $ [c | ((r, c), _) <- M.toList maze, r == row])
getWrapAroundPoint maze ((_, col), DD) = (minimum $ [r | ((r, c), _) <- M.toList maze, c == col], col)

getNextPoint :: Maze -> Position -> Point
getNextPoint maze position@(point, dir)
    | M.member nextPoint maze = nextPoint
    | otherwise = getWrapAroundPoint maze position
    where nextPoint = next point dir

getStartingPoint :: Maze -> Point
getStartingPoint maze = case M.lookup startingPoint maze of
    Nothing -> error "starting point not found"
    Just _ -> startingPoint
    where minRow = minimum [row | ((row, _), _) <- M.toList maze]
          minCol = minimum [col | ((row, col), _) <- M.toList maze, row == minRow]
          startingPoint = (minRow, minCol)

walk :: Maze -> Position -> Instruction -> Position
walk maze (point, dir) (Rotate RL) = tr (point, csucc dir)
walk maze (point, dir) (Rotate RR) = tr (point, cpred dir)
walk maze pos@(point, dir) Walk = case M.lookup nextPoint maze of
    Nothing -> error $ "next point doesn't exist: " ++ show pos
    Just Tile -> tr (nextPoint, dir)
    Just Wall -> tr pos
    where nextPoint = getNextPoint maze pos

main = do
  (maze, instructions) <- parseInput <$> TIO.readFile "input.txt"
  print maze
  print instructions
  print $ calcPassword $ foldl' (walk maze) (getStartingPoint maze, DR) instructions
