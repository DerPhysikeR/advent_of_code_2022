import Data.Char (ord, isLower, isUpper)
data Rucksack = Rucksack {c1 :: String, c2 :: String} deriving (Show)

parseInput :: String -> [Rucksack]
parseInput s = map parseLine $ lines s
    where parseLine line = Rucksack (take half line) (drop half line)
            where half = div (length line) 2

getPriority :: Char -> Int
getPriority l
    | isLower l = ord l - 96
    | isUpper l = ord l - 38

findCommonItem :: Rucksack -> Char
findCommonItem Rucksack {c1, c2} = head $ filter (`elem` c1) c2

main :: IO ()
main = do
    rucksacks <- parseInput <$> readFile "input.txt"
    print $ sum $ map (getPriority . findCommonItem) rucksacks
