module Day09 where

import Data.List.Split (splitOn)
import Debug.Trace (trace)

main :: IO ()
main = do
  content <- readFile "inputs/day09.txt"
  let input = build content
  print $ resolve1 input
  print $ resolve2 input

build :: String -> [(Int,Int)]
build = map (make_tuple) . lines

make_tuple :: String -> (Int, Int)
make_tuple s = (read n1, read n2)
  where
    [n1, n2] = splitOn "," s

resolve1 :: [(Int, Int)] -> Int
resolve1 ls = maximum [area (ls !! i) (ls !! j) | i <- [0.. len - 1], j <- [i+1.. len -1]]
  where
    len = length ls

area :: (Int, Int) -> (Int, Int) -> Int
area (a,b) (c,d) = (abs1 + 1) * (abs2 + 1)
  where
    abs1 = abs a - c
    abs2 = abs b - d

resolve2 :: [(Int, Int)] -> Int
resolve2 ls = maximum [area (ls !! i) (ls !! j) | i <- [0.. len - 1], j <- [i+1.. len -1], in_fenze (ls !! i) (ls !! j) ls, point_in_rect ls $ mid_point (ls !! i) (ls !! j)]
  where
    len = length ls

in_fenze :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Bool
in_fenze a b [] = trace (show a ++ show b) True
in_fenze (a,b) (c,d) ((x,y):xs)
  | (acot a x c && acot b y d) = False
  | otherwise = in_fenze (a,b) (c,d) xs

point_in_rect :: [(Int,Int)] -> (Int, Int) -> Bool
point_in_rect ls (x, y) = trace ("rect"++show (x,y)) length [i | i <- [0..len-1], y < (snd (ls !! i)), acot (fst (ls !! (i `mod` len))) x (fst (ls !! ((i + 1) `mod` len)))] `mod` 2 == 1
  where
    len = length ls

mid_point :: (Int, Int) -> (Int, Int) -> (Int, Int)
mid_point (a,b) (c,d) = ((a+c) `div` 2, (b+d) `div` 2)

acot :: Ord a => a -> a -> a -> Bool
acot a b c = ((a < b) && (b < c)) || ((c < b) && (b < a))
