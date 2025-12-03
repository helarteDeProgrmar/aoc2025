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
resolve2 = sum . map (\s -> search_max_improved s [0 | _ <- [1 .. 12]])

search_max_improved :: String -> [Int] -> Int
search_max_improved "" l = build_number $ reverse l
search_max_improved (x : xs) l = search_max_improved xs $ build_list_by_len (read [x]) (length xs) l

build_list_by_len :: Int -> Int -> [Int] -> [Int]
build_list_by_len n len l = take sep l ++ build_list n (drop sep l)
  where
    sep = separator (length l) len

build_list :: Int -> [Int] -> [Int]
build_list n l
  | index >= len = l
  | otherwise = take index l ++ [n] ++ [0 | _ <- [1 .. len - index - 1]]
  where
    len = length l
    index = search_index 0 n l

search_index :: Int -> Int -> [Int] -> Int
search_index acc _ [] = acc + 1
search_index acc n (x : xs)
  | n > x = acc
  | otherwise = search_index (acc + 1) n xs

separator :: Int -> Int -> Int
separator amo len
  | len >= amo = 0
  | otherwise = amo - (len + 1)

build_number :: [Int] -> Int
build_number [] = 0
build_number (x : xs) = x + 10 * build_number xs
