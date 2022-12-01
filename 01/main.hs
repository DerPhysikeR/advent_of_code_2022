import Data.List (sort)

f :: String -> [[Int]] -> [[Int]]
f [] acc = [] : acc
f n acc = (read n : head acc) : tail acc

parseInputFile :: [String] -> [[Int]]
parseInputFile = foldr f [[]]

main :: IO ()
main = do
  input <- parseInputFile . lines <$> readFile "input.txt"
  let calories = map sum input
  print $ maximum calories
  print $ sum . take 3 . reverse . sort $ calories
