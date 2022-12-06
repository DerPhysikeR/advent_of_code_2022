import Control.Monad (unless)
import Data.Maybe (isNothing)

allDifferent :: String -> Bool
allDifferent [] = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe n xs = if len < n then Nothing else Just taken
    where len = length taken
          taken = take n xs

findFirstInstanceOfNDifferent :: Int -> Int -> String -> Maybe Int
findFirstInstanceOfNDifferent n i xs = case takeMaybe n xs of
    Nothing -> Nothing
    Just ys -> if allDifferent ys then Just (i + n - 1) else findFirstInstanceOfNDifferent n (i + 1) (tail xs)

findStartOfPacket = findFirstInstanceOfNDifferent 4
findStartOfMessage = findFirstInstanceOfNDifferent 14

main = do
    -- tests
    let error_message = "error in findStartOfPacket"
    unless (findStartOfPacket 1 "abcd" == Just 4) (error error_message)
    unless (findStartOfPacket 1 "bvwbjplbgvbhsrlpgdmjqwftvncz" == Just 5) (error error_message)
    unless (findStartOfPacket 1 "nppdvjthqldpwncqszvftbrmjlhg" == Just 6) (error error_message)
    unless (findStartOfPacket 1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == Just 7) (error error_message)
    unless (findStartOfPacket 1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == Just 10) (error error_message)
    unless (findStartOfPacket 1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == Just 11) (error error_message)
    unless (isNothing $ findStartOfPacket 1 "aaaaaaaaaaaaaaaa") (error error_message)
    unless (isNothing $ findStartOfPacket 1 "abc") (error error_message)

    let error_message = "error in findStartOfMessage"
    unless (isNothing $ findStartOfMessage 1 "abc") (error error_message)
    unless (findStartOfMessage 1 "abcdefghijklmn" == Just 14) (error error_message)
    unless (findStartOfMessage 1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == Just 19) (error error_message)
    unless (findStartOfMessage 1 "bvwbjplbgvbhsrlpgdmjqwftvncz" == Just 23) (error error_message)
    unless (findStartOfMessage 1 "nppdvjthqldpwncqszvftbrmjlhg" == Just 23) (error error_message)
    unless (findStartOfMessage 1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == Just 29) (error error_message)
    unless (findStartOfMessage 1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == Just 26) (error error_message)

    -- main
    input <- head . lines <$> readFile "input.txt"
    print $ findStartOfPacket 1 input
    print $ findStartOfMessage 1 input
