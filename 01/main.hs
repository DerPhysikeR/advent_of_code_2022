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
  print $ maximum $ map sum content
