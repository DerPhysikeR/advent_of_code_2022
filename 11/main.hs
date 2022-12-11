{-# LANGUAGE OverloadedStrings #-}
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Sequence qualified as S
import Data.Foldable (Foldable(foldl'), toList)
import Data.Foldable qualified as F
import Data.List (sort)

data Monkey = Monkey {items :: [Int], operation :: Int -> Int, throwTo :: Int -> Int, activity :: Int}
type Monkeys = S.Seq Monkey

instance Show Monkey where
    show (Monkey items operation throwTo activity) = "Monkey " ++ show activity

parseMonkey :: T.Text -> Monkey
parseMonkey t = Monkey items operation (\x -> if (x `mod` divisor) == 0 then trueMonkey else falseMonkey) 0
    where (_:itemLine:opLine:testLine:ifTrueLine:ifFalseLine:_) = T.lines t
          afterColon = last . T.splitOn ": "
          afterOld = last . T.splitOn " old "
          ru = read . T.unpack
          items = map ru $ T.splitOn ", " $ afterColon itemLine
          operation = case T.splitOn " " $ afterOld opLine of
            ("*":"old":_) -> \x -> x * x
            ("+":"old":_) -> \x -> x + x
            ("*":num:_) -> (ru num *)
            ("+":num:_) -> (ru num +)
          divisor = ru . last $ T.splitOn " " testLine
          trueMonkey = ru . last $ T.splitOn " " ifTrueLine
          falseMonkey = ru . last $ T.splitOn " " ifFalseLine

monkeyThrow :: (Int -> Int) -> Int -> Monkeys -> Monkeys
monkeyThrow calcWorryLevel idx monkeys = case S.index monkeys idx of
    Monkey [] _ _ _ -> monkeys
    Monkey (item:items) operation throwTo activity -> monkeysItemCaught
        where monkeysItemThrown = S.update idx (Monkey items operation throwTo (activity + 1)) monkeys
              worryLevel = calcWorryLevel (operation item)
              throwToIdx = throwTo worryLevel
              catchingMonkey@(Monkey it o tt a) = S.index monkeysItemThrown throwToIdx
              monkeysItemCaught = S.update throwToIdx (Monkey (it ++ [worryLevel]) o tt a) monkeysItemThrown

monkeyTurn :: (Int -> Int) -> Monkeys -> Int -> Monkeys
monkeyTurn calcWorryLevel monkeys idx = case S.index monkeys idx of
    Monkey [] _ _ _ -> monkeys
    _ -> monkeyTurn calcWorryLevel (monkeyThrow calcWorryLevel idx monkeys) idx

monkeyRound :: (Int -> Int) -> Monkeys -> Monkeys
monkeyRound calcWorryLevel monkeys = foldl' (monkeyTurn calcWorryLevel) monkeys [0..length monkeys - 1]

calcWorryLevelPart1 x = div x 3
calcWorryLevelPart2 x = mod x (2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23)

solve :: (Int -> Int) -> Int -> Monkeys -> Monkeys
solve calcWorryLevel rounds monkeys = last $ take rounds $ iterate (monkeyRound calcWorryLevel) monkeys

calcMonkeyBusiness :: Monkeys -> Int
calcMonkeyBusiness monkeys = product $ take 2 $ reverse $ sort $ map (\(Monkey _ _ _ activity) -> activity) (F.toList monkeys)

main :: IO ()
main = do
    monkeys <- S.fromList . map parseMonkey . T.splitOn "\n\n" <$> TIO.readFile "input.txt"
    let finishedMonkeys = solve calcWorryLevelPart1 21 monkeys
    print $ calcMonkeyBusiness finishedMonkeys
    let finishedMonkeys = solve calcWorryLevelPart2 10001 monkeys
    print $ calcMonkeyBusiness finishedMonkeys
