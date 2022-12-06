import Control.Monad (unless)
import Data.Maybe (isNothing)

allDifferent :: String -> Bool
allDifferent [] = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe n xs = if len < n then Nothing else Just taken
    where len = length taken
          taken = take n xs

findMarker :: Int -> Int -> String -> Maybe Int
findMarker n i xs = case takeMaybe n xs of
    Nothing -> Nothing
    Just ys -> if allDifferent ys then Just (i + n - 1) else findMarker n (i + 1) (tail xs)

findStartOfPacket = findMarker 4 1
findStartOfMessage = findMarker 14 1

main = do
    -- tests
    let error_message = "error in findStartOfPacket"
    unless (findStartOfPacket "abcd" == Just 4) (error error_message)
    unless (findStartOfPacket "bvwbjplbgvbhsrlpgdmjqwftvncz" == Just 5) (error error_message)
    unless (findStartOfPacket "nppdvjthqldpwncqszvftbrmjlhg" == Just 6) (error error_message)
    unless (findStartOfPacket "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == Just 7) (error error_message)
    unless (findStartOfPacket "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == Just 10) (error error_message)
    unless (findStartOfPacket "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == Just 11) (error error_message)
    unless (isNothing $ findStartOfPacket "aaaaaaaaaaaaaaaa") (error error_message)
    unless (isNothing $ findStartOfPacket "abc") (error error_message)

    let error_message = "error in findStartOfMessage"
    unless (isNothing $ findStartOfMessage "abc") (error error_message)
    unless (findStartOfMessage "abcdefghijklmn" == Just 14) (error error_message)
    unless (findStartOfMessage "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == Just 19) (error error_message)
    unless (findStartOfMessage "bvwbjplbgvbhsrlpgdmjqwftvncz" == Just 23) (error error_message)
    unless (findStartOfMessage "nppdvjthqldpwncqszvftbrmjlhg" == Just 23) (error error_message)
    unless (findStartOfMessage "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == Just 29) (error error_message)
    unless (findStartOfMessage "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == Just 26) (error error_message)

    -- main
    input <- head . lines <$> readFile "input.txt"
    print $ findStartOfPacket input
    print $ findStartOfMessage input
