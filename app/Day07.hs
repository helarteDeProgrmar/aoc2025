module Day07 where

import Debug.Trace (trace)
main :: IO ()
main = do
  content <- readFile "inputs/day07.txt"
  let (start:continue) = lines content
  print $ resolve1 start continue 0
  print $ resolve2 start continue 0

resolve1 :: String -> [String] -> Int -> Int
resolve1 _ [] total = total
resolve1 current (x:xs) acc = resolve1 next_gen xs (acc + bif)
  where
    (next_gen, bif) = next current x

next :: String -> String -> (String, Int)
next current next_gen = trace (show sol) sol
  where
    sol = ([prepare_char i | i <- [0 .. len - 1]], length [i | i<- [0.. len - 1], current !! i == '|', next_gen !! i == '^'])
    len = length current
    prepare_char i
      | next_gen !! i == '^' = '^'
      | (i-1) >= 0 && next_gen !! (i-1) == '^' && current !! (i-1) == '|' = '|'
      | (i+1) < len && next_gen !! (i+1) == '^' && current !! (i+1) == '|' = '|'
      | current !! i == 'S' = '|'
      | current !! i == '|' = '|'
      | otherwise = '.'
