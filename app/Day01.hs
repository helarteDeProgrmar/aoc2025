module Day01 where

main :: IO ()
main = do
  content <- readFile "inputs/day01.txt"
  let trans = build (\x -> (head x, read $ tail x)) content
  print $ resolve1 50 trans
  print $ resolve2 50 trans

build :: (String -> (Char, Int)) -> String -> [(Char, Int)]
build fun str = map fun $ lines str

resolve1 :: Int -> [(Char, Int)] -> Int
resolve1 n [] = 0
resolve1 n (x : xs) =
  let new_counter = counter n x
   in case new_counter of
        0 -> 1 + resolve1 new_counter xs
        _ -> resolve1 new_counter xs

counter :: Int -> (Char, Int) -> Int
counter n ('R', c) = (n + c) `mod` 100
counter n ('L', c) = (n - c) `mod` 100

loops :: Int -> (Char, Int) -> Int
loops n ('R', c) = length [x | x <- [n + 1 .. n + c], x `mod` 100 == 0]
loops n ('L', c) = length [x | x <- [n - c .. n - 1], x `mod` 100 == 0]

resolve2 :: Int -> [(Char, Int)] -> Int
resolve2 n [] = 0
resolve2 n (x : xs) =
  let new_counter = counter n x
      amount_loops = loops n x
   in amount_loops + resolve2 new_counter xs
