module Day07 where

main :: IO ()
main = do
  content <- readFile "inputs/day07-2.txt"
  let (start, continue) = lines content
  print $ resolve1 start continue

resolve1 :: String -> [String] -> Int
resolve1 current [] = length $ filter (\x -> x == '|') current
resolve1 current (x:xs) = resolve1 (next current x) xs

next :: String -> String
next current next = [prepare_char i | i <- [0 .. length current - 1]]
  where
    prepare_char i
      | current !! i == '^' = '.'
      | next !! i - 1 == '^' && i
      | current !! i + 1 == '^'
