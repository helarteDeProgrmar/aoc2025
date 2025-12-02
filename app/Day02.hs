module Day02 where

import Data.List (splitAt)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- readFile "inputs/day02.txt"
  let input = build content
  print input
  print $ resolve1 input
  print $ resolve2 input

build :: String -> [(Int, Int)]
build s = map (\(x : y : _) -> (read x, read y)) $ map (splitOn "-") $ splitOn "," s

resolve1 :: [(Int, Int)] -> Int
resolve1 list = sum $ map (\(a, b) -> sum [x | x <- [a .. b], is_invalid $ show x]) list

-- is_invalid :: String -> Bool
-- is_invalid "" = True
-- is_invalid (x : xs) =
--   case unsnoc xs of
--     Nothing -> False
--     Just (middle, lastChar) -> and [x == lastChar, is_invalid middle]
is_invalid :: String -> Bool
is_invalid s =
  let len = length s
      (a, b) = splitAt (len `div` 2) s
   in case even len of
        False -> False
        True -> a == b

new_is_invalid :: String -> Bool
new_is_invalid s = or [has_n_sequence i "" s | i <- [1 .. len `div` 2], len `mod` i == 0] where len = length s

has_n_sequence :: Int -> String -> String -> Bool
has_n_sequence _ _ "" = True
has_n_sequence n "" s = has_n_sequence n a b where (a, b) = splitAt n s
has_n_sequence n seq str =
  let (a, b) = splitAt n str
   in and [seq == a, has_n_sequence n seq b]

resolve2 :: [(Int, Int)] -> Int
resolve2 list = sum $ map (\(a, b) -> sum [x | x <- [a .. b], new_is_invalid $ show x]) list

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
