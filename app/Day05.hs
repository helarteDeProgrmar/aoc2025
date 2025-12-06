module Day05 where

import Data.List.Split (splitOn)
import Debug.Trace (trace)

main :: IO ()
main = do
    content <- readFile "inputs/day05.txt"
    let (intervals, ids) = build content
    print $ resolve1 intervals ids
    print $ resolve2 intervals

build :: String -> ([(Int, Int)], [Int])
build s = (build_intervals s1, build_ids s2)
  where
    [s1, s2] = splitOn "\n\n" s

build_intervals :: String -> [(Int, Int)]
build_intervals s = trace (show norm) norm
  where
    norm = normalize_intervals raw_intervals []
    raw_intervals = map (make_interval) $ lines s

make_interval :: String -> (Int, Int)
make_interval s = (read s1, read s2)
  where
    [s1, s2] = splitOn "-" s

normalize_intervals :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
normalize_intervals [] l = l
normalize_intervals (x:xs) l = normalize_intervals xs (add_interval x l)

add_interval :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
add_interval interval [] = [interval]
add_interval (a, b) ((c, d):xs)
  | c <= a && b <= d = ((c,d):xs)
  | a <= c && c <= b && b <= d = add_interval (a,d) xs
  | c <= a && a <= d && d <= b = add_interval (c,b) xs
  | a <= c && d <= b = add_interval (a,b) xs
  | otherwise = [(c,d)] ++ add_interval (a,b) xs

build_ids :: String -> [Int]
build_ids = map (read) . lines

resolve1 :: [(Int, Int)] -> [Int] -> Int
resolve1 inters ids = length $ filter (\x -> interval_in_list x inters) ids

interval_in_list :: Int -> [(Int, Int)] -> Bool
interval_in_list _ [] = False
interval_in_list i ((a, b):xs)
  | i >= a && i <= b = True
  | otherwise = interval_in_list i xs

resolve2 :: [(Int, Int)] -> Int
resolve2 = sum . map (\(a,b) -> b - a + 1)
