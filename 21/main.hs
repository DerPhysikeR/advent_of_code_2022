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

main :: IO ()
main = do
    nodes <- M.fromList . map parseLine . T.lines <$> TIO.readFile "input.txt"
    print nodes
    print $ evaluate nodes "root"
