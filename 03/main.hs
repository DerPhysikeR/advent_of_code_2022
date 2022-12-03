import Data.Char (ord, isLower, isUpper)

type Rucksack = String
type Group = (Rucksack, Rucksack, Rucksack)

getPriority :: Char -> Int
getPriority l
    | isLower l = ord l - 96
    | isUpper l = ord l - 38

findCommonLetters :: String -> String -> String
findCommonLetters s1 = filter (`elem` s1)

findCommonLetter :: String -> String -> Char
findCommonLetter s1 s2 = head $ findCommonLetters s1 s2

findCommonItem :: Rucksack -> Char
findCommonItem rucksack = findCommonLetter c1 c2
    where half = div (length rucksack) 2
          c1 = take half rucksack
          c2 = drop half rucksack

findCommonGroupItem :: Group -> Char
findCommonGroupItem (r1, r2, r3) = findCommonLetter r1 $ findCommonLetters r2 r3

groupRucksacks :: [Rucksack] -> [Group]
groupRucksacks [] = []
groupRucksacks (r1:r2:r3:rs) = (r1, r2, r3) : groupRucksacks rs

main :: IO ()
main = do
    rucksacks <- lines <$> readFile "input.txt"
    print $ sum $ map (getPriority . findCommonItem) rucksacks
    print $ sum $ map (getPriority . findCommonGroupItem) $ groupRucksacks rucksacks
