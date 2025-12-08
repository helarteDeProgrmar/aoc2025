module Day06 where

import Debug.Trace (trace)

main :: IO ()
main = do
  content <- readFile "inputs/day06.txt"
  let (numbers, ops) = build content
  print $ resolve1 numbers ops
  print $ resolve1 numbers ops

build :: String -> ([[Int]], [String])
build s = (map (\x -> (map (read) $ words x)) s_numbers, words s_ops)
  where
    ls = lines s
    Just (s_numbers, s_ops) = unsnoc ls

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

resolve1 :: [[Int]] -> [String] -> Int
resolve1 _ [] = 0
resolve1 nums ops = calculate_line (map (head) nums) (head ops) + resolve1 (map (tail) nums) (tail ops)

calculate_line :: [Int] -> String -> Int
calculate_line nums "*" = foldl (*) 1 nums
calculate_line nums _ = sum nums
