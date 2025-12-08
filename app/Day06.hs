module Day06 where

import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- readFile "inputs/day06.txt"
  let (numbers, ops, other_nums) = build content
  print $ resolve1 numbers ops
  print $ resolve2 other_nums ops

build :: String -> ([[Int]], [String], [[Int]])
build s = (map (\x -> (map (read) $ words x)) s_numbers, words s_ops, corrected_lists)
  where
    ls = lines s
    Just (s_numbers, s_ops) = unsnoc ls
    lines_nums = build_nums s_numbers
    corrected_lists = splitOn [0] lines_nums

build_nums :: [String] -> [Int]
build_nums ("":_) = []
build_nums ls = [build_number $ filter (\x -> x /= " ") $ reverse $ map (\x -> [head x]) ls] ++ (build_nums $ map (tail) ls)

build_number :: [String] -> Int
build_number [] = 0
build_number (c:xs) = (read c) + 10 * build_number xs

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

resolve1 :: [[Int]] -> [String] -> Int
resolve1 _ [] = 0
resolve1 nums ops = calculate_line (map (head) nums) (head ops) + resolve1 (map (tail) nums) (tail ops)

calculate_line :: [Int] -> String -> Int
calculate_line nums "*" = foldl (*) 1 nums
calculate_line nums _ = sum nums

resolve2 :: [[Int]] -> [String] -> Int
resolve2  [] _ = 0
resolve2 (i:xs) (o:ops) = calculate_line i o + resolve2 xs ops
