import Data.List (sort)
parseInputFile :: [String] -> [[Int]]
parseInputFile =
  foldr
    ( \x acc -> case x of
        [] -> [] : acc
        n -> (read n : head acc) : tail acc
    )
    [[]]

main :: IO ()
main = do
  content <- parseInputFile . lines <$> readFile "input.txt"
  let calories = map sum content
  print $ maximum calories
  print $ sum . take 3 . reverse . sort $ calories
