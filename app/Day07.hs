module Day07 where

import Data.List (elemIndex)

main :: IO ()
main = do
  content <- readFile "inputs/day07.txt"
  let (start:continue) = lines content
  print $ resolve1 start continue 0
  let counts = map (\x -> if x then 1 else 0) [start !! i == 'S' | i <- [0..length start-1]]
  print $ resolve2 counts continue

resolve1 :: String -> [String] -> Int -> Int
resolve1 _ [] total = total
resolve1 current (x:xs) acc = resolve1 next_gen xs (acc + bif)
  where
    (next_gen, bif) = next current x

next :: String -> String -> (String, Int)
next current next_gen = ([prepare_char i | i <- [0 .. len - 1]], length [i | i<- [0.. len - 1], current !! i == '|', next_gen !! i == '^'])
  where
    len = length current
    prepare_char i
      | next_gen !! i == '^' = '^'
      | (i-1) >= 0 && next_gen !! (i-1) == '^' && current !! (i-1) == '|' = '|'
      | (i+1) < len && next_gen !! (i+1) == '^' && current !! (i+1) == '|' = '|'
      | current !! i == 'S' = '|'
      | current !! i == '|' = '|'
      | otherwise = '.'

resolve2 counts [] = sum counts
resolve2 counts (s:ss) = resolve2 [left_add i + mid_add i + right_add i| i <- [0..len -1]] ss
  where
    len = length counts
    mid_add i
      | s !! i == '^' = 0
      | otherwise = counts !! i
    left_add i
      | i == 0 = 0
      | s !! (i - 1) == '^' = counts !! (i-1)
      | otherwise = 0
    right_add i
      | i == len - 1 = 0
      | s !! (i + 1) == '^' = counts !! (i+1)
      | otherwise = 0
