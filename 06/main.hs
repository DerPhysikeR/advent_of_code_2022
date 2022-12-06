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
    input <- head . lines <$> readFile "input.txt"
    print $ findStartOfPacket input
    print $ findStartOfMessage input
