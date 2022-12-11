{-# LANGUAGE OverloadedStrings #-}
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Sequence qualified as S
import Debug.Trace (traceShow)
import Data.Foldable (Foldable(foldl'), toList)
import Data.Foldable qualified as F
import Data.List (sort)
tr x = traceShow x x

data Monkey = Monkey {items :: [Int], operation :: Int -> Int, throwTo :: Int -> Int, activity :: Int}
type Monkeys = S.Seq Monkey

instance Show Monkey where
    show (Monkey items operation throwTo activity) = "Monkey " ++ show items ++ " " ++ show activity

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

monkeyThrow :: Int -> Monkeys -> Monkeys
monkeyThrow idx monkeys = case S.index monkeys idx of
    Monkey [] _ _ _ -> monkeys
    Monkey (item:items) operation throwTo activity -> monkeysItemCaught
        where monkeysItemThrown = S.update idx (Monkey items operation throwTo (activity + 1)) monkeys
              worryLevel = div (operation item) 3
              throwToIdx = throwTo worryLevel
              catchingMonkey@(Monkey it o tt a) = S.index monkeysItemThrown throwToIdx
              monkeysItemCaught = S.update throwToIdx (Monkey (it ++ [worryLevel]) o tt a) monkeysItemThrown

monkeyTurn :: Int -> Monkeys -> Monkeys
monkeyTurn idx monkeys = case S.index monkeys idx of
    Monkey [] _ _ _ -> monkeys
    _ -> monkeyTurn idx (monkeyThrow idx monkeys)

monkeyRound :: Monkeys -> Monkeys
monkeyRound monkeys = foldl' (flip monkeyTurn) monkeys [0..length monkeys - 1]

main :: IO ()
main = do
    monkeys <- S.fromList . map parseMonkey . T.splitOn "\n\n" <$> TIO.readFile "input.txt"
    print monkeys
    let finishedMonkeys = last $ take 21 $ iterate monkeyRound monkeys
    print $ product $ take 2 $ reverse $ sort $ map (\(Monkey _ _ _ activity) -> activity) (F.toList finishedMonkeys)