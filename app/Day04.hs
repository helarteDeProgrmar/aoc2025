module Day04 where

import Data.Array
import Debug.Trace (trace)

main :: IO ()
main = do
  content <- readFile "inputs/day04.txt"
  let input = build content
  print $ resolve1 input
  print $ resolve2 input 1

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


resolve1 :: Array (Int, Int) Char -> Int
resolve1 arr = length $ filter (< 4) numbers_role
  where
    numbers_role = [count_adjacents_rolls arr idx | idx <- indices arr, arr ! idx == '@']

count_adjacents_rolls :: Array (Int, Int) Char -> (Int, Int) -> Int
count_adjacents_rolls arr (a, b) = length $ filter (== '@') [arr ! i | i <- adjacents_idx]
  where
    adjacents_idx = [(i, j) | i <- [a - 1 .. a + 1], j <- [b - 1 .. b + 1], (i, j) /= (a, b), in_bounds (i, j)]
    in_bounds (i, j) = i > 0 && j > 0 && i <= rows && j <= cols
    (_, (rows, cols)) = bounds arr

resolve2 :: Array (Int, Int) Char -> Int -> Int
resolve2 arr 0 = -1
resolve2 arr n = n + (resolve2 (transform arr) $ resolve1 arr)

transform :: Array (Int, Int) Char -> Array (Int, Int) Char
transform arr = array
                  (bounds arr)
                  [ ((i, j), decide_char (i,j)) | (i,j) <- indices arr]
                where
                  decide_char idx = if count_adjacents_rolls arr idx < 4 then '.' else arr ! idx
