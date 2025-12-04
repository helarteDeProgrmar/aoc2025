module Day04 where

import Data.Array
import Debug.Trace (trace)

main :: IO ()
main = do
  content <- readFile "inputs/day04-2.txt"
  let input = build content
  print $ resolve1 input
  print $ resolve1_aux input
  print $ resolve2 input

build :: String -> Array (Int, Int) Char
build s =
  array
    ((1, 1), (rows, columns))
    [ ((i, j), (ls !! (i - 1)) !! (j - 1))
      | i <- [1 .. rows],
        j <- [1 .. columns]
    ]
  where
    ls = lines s
    rows = length ls
    columns = length $ head ls

resolve1_aux :: Array (Int, Int) Char -> Int
resolve1_aux arr = length $ [idx | idx <- indices arr, arr ! idx /= '@']

resolve1 :: Array (Int, Int) Char -> Int
resolve1 arr = length $ filter (< 4) $ trace (show $ length numbers_role) numbers_role
  where
    numbers_role = [count_adjacents_rolls arr idx | idx <- indices arr, arr ! idx == '@']

count_adjacents_rolls :: Array (Int, Int) Char -> (Int, Int) -> Int
count_adjacents_rolls arr (a, b) = length $ filter (== '@') [arr ! i | i <- adjacents_idx]
  where
    adjacents_idx = [(i, j) | i <- [a - 1 .. a + 1], j <- [b - 1 .. b + 1], (i, j) /= (a, b), in_bounds (i, j)]
    in_bounds (i, j) = i > 0 && j > 0 && i < rows && j < cols
    (_, (rows, cols)) = bounds arr

resolve2 :: Array (Int, Int) Char -> Int
resolve2 _ = 1
