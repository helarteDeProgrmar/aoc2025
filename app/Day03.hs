module Day03 where

main :: IO ()
main = do
  content <- readFile "inputs/day03.txt"
  let input = build content
  print $ resolve1 input
  print $ resolve2 input

build :: String -> [String]
build = lines

resolve1 :: [String] -> Int
resolve1 = sum . map (\s -> search_max s 0 0)

search_max :: String -> Int -> Int -> Int
search_max "" a b = a * 10 + b
search_max [c] a b
  | c_int > b = a * 10 + c_int
  | otherwise = a * 10 + b
  where
    c_int = read [c]
search_max (x : xs) a b
  | x_int > a = search_max xs x_int 0
  | x_int > b = search_max xs a x_int
  | otherwise = search_max xs a b
  where
    x_int = read [x]

resolve2 :: [String] -> Int
resolve2 _ = -1
