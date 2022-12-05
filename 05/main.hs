{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text(..))
import Data.Text qualified as T
import Data.Text.IO qualified as IO
import Data.Sequence (Seq(..))
import Data.Sequence qualified as S
import Data.Char (isSpace)
import Data.List (transpose)
import Data.Maybe (catMaybes)

type Stack = [Char]
type Stacks = Seq Stack
data Move = Move {mCount :: Int, mFrom :: Int, mTo :: Int} deriving (Show)
type Moves = [Move]

parseMove :: Text -> Move
parseMove t = Move (ru count) (ru from - 1) (ru to - 1)
    where ru = read . T.unpack
          (_:count:_:from:_:to:_) = T.splitOn " " t

parseStackLine :: Int -> String -> [Maybe Char]
parseStackLine max s = [let l = s!!i in if isSpace l then Nothing else Just l | i <- [1,5..max]]

parseStacks :: Text -> Stacks
parseStacks t = stacks
    where allLines = T.lines t
          stackLines = map (parseStackLine (T.length $ last allLines) . T.unpack) (init allLines)
          stacks = S.fromList $ map catMaybes (transpose stackLines)

parseInput :: Text -> (Stacks, Moves)
parseInput t = (stacks, moves)
    where (firstPart:secondPart:_) = T.splitOn "\n\n" t
          stacks = parseStacks firstPart
          moves = map parseMove (T.lines secondPart)

moveBox :: Int -> Int -> Stacks -> Stacks
moveBox from to s = S.update to (head oldStack : newStack) removed
    where oldStack = S.index s from
          newStack = S.index s to
          removed = S.update from (tail oldStack) s

applyMove :: Stacks -> Move -> Stacks
applyMove s (Move count from to) = iterate (moveBox from to) s !! count

moveMultipleBoxes :: Stacks -> Move -> Stacks
moveMultipleBoxes s (Move count from to) = removed
    where oldStack = S.index s from
          newStack = S.index s to
          added = S.update to (take count oldStack ++ newStack) s
          removed = S.update from (drop count oldStack) added

main :: IO ()
main = do
    (stacks, moves) <- parseInput <$> IO.readFile "input.txt"
    print $ head <$> foldl applyMove stacks moves
    print $ head <$> foldl moveMultipleBoxes stacks moves
