{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Char (isDigit)
import qualified Data.Map as M

data Node = Number Int | Operation String Char String deriving (Show)

parseNode :: T.Text -> Node
parseNode t
    | isDigit $ T.head t = Number $ read $ T.unpack t
    | otherwise = case T.splitOn " " t of
        (first:"+":second:_) -> Operation (T.unpack first) '+' (T.unpack second)
        (first:"-":second:_) -> Operation (T.unpack first) '-' (T.unpack second)
        (first:"*":second:_) -> Operation (T.unpack first) '*' (T.unpack second)
        (first:"/":second:_) -> Operation (T.unpack first) '/' (T.unpack second)
        _ -> error $ T.unpack t

parseLine :: T.Text -> (String, Node)
parseLine line = (T.unpack name, parseNode rest)
    where (name:rest:_) = T.splitOn ": " line

eval :: Char -> Int -> Int -> Int
eval '+' = (+)
eval '-' = (-)
eval '*' = (*)
eval '/' = div

evaluate :: M.Map String Node -> String -> Int
evaluate m s = case M.lookup s m of
    Nothing -> error s
    Just (Number x) -> x
    Just (Operation first op second) -> eval op (evaluate m first) (evaluate m second)

check :: M.Map String Node -> Bool
check m = case M.lookup "root" m of
    Nothing -> error "could not find 'root' in map"
    Just (Number x) -> error "root should not contain a number"
    Just (Operation first op second) -> evaluate m first == evaluate m second

eval2 :: Char -> Maybe Int -> Maybe Int -> Maybe Int
eval2 _ Nothing _ = Nothing
eval2 _ _ Nothing = Nothing
eval2 '+' (Just x) (Just y) = Just (x + y)
eval2 '-' (Just x) (Just y) = Just (x - y)
eval2 '*' (Just x) (Just y) = Just (x * y)
eval2 '/' (Just x) (Just y) = Just (div x y)

evaluate2 :: M.Map String Node -> String -> Maybe Int
evaluate2 m s = case M.lookup s m of
    Nothing -> error s
    Just (Number x) -> if s == "humn" then Nothing else Just x
    Just (Operation first op second) -> eval2 op (evaluate2 m first) (evaluate2 m second)

solve2 :: M.Map String Node -> String -> Int -> Int
solve2 m "humn" i = i
solve2 m s i = case M.lookup s m of
    Nothing -> error s
    Just (Number x) -> error s
    Just (Operation first op second) -> case evaluate2 m first of
        Nothing -> solve2 m first (eval2First i op (evaluate m second))
        Just x -> solve2 m second (eval2Second i op x)

eval2First :: Int -> Char -> Int -> Int
eval2First i '+' x = i - x
eval2First i '-' x = i + x
eval2First i '*' x = div i x
eval2First i '/' x = i * x

eval2Second :: Int -> Char -> Int -> Int
eval2Second i '+' x = i - x
eval2Second i '-' x = x - i
eval2Second i '*' x = div i x
eval2Second i '/' x = div x i

part2 :: M.Map String Node -> Int
part2 m = case M.lookup "root" m of
    Nothing -> error "root"
    Just (Number _) -> error ""
    Just (Operation first op second) -> case evaluate2 m first of
        Nothing -> solve2 m first (evaluate m second)
        Just x -> solve2 m second x

main :: IO ()
main = do
    nodes <- M.fromList . map parseLine . T.lines <$> TIO.readFile "input.txt"
    print $ evaluate nodes "root"
    print $ part2 nodes
