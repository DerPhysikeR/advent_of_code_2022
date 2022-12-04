{-# LANGUAGE OverloadedStrings #-}
import Data.Text qualified as T
import Data.Text.IO qualified as IO

data Range = Range {start :: Int, end :: Int} deriving (Show)
type Pair = (Range, Range)

isFullyInside :: Range -> Range -> Bool
isFullyInside r1 r2 = (start r2 <= start r1) && (end r1 <= end r2)

isFullyDistinct :: Range -> Range -> Bool
isFullyDistinct r1 r2 = (end r1 < start r2) || (end r2 < start r1)

parseRange :: T.Text -> Range
parseRange t = Range (toInt start) (toInt end)
    where (start:end:_) = T.splitOn "-" t
          toInt = read . T.unpack

parsePair :: T.Text -> Pair
parsePair t = (p1, p2)
    where(p1:p2:_) = map parseRange (T.splitOn "," t)

main :: IO ()
main = do
    pairs <- map parsePair . T.lines <$> IO.readFile "input.txt"
    print $ sum $ map (fromEnum . (\(r1, r2) -> isFullyInside r1 r2 || isFullyInside r2 r1)) pairs
    print $ sum $ map (fromEnum . not . uncurry isFullyDistinct) pairs
