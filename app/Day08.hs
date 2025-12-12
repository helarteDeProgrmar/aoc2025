module Day08 where

import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Data.List (sort, sortOn)
import Debug.Trace (trace)

main :: IO ()
main = do
  content <- readFile "inputs/day08.txt"
  let input = build content
  print $ resolve1 input 10
  print $ resolve2 input

build :: String -> [(Int, Int, Int)]
build = map (build_line) . lines

build_line :: String -> (Int, Int, Int)
build_line l = (i1, i2, i3)
  where
    [i1,i2,i3] = map (read) $ splitOn "," l

resolve1 :: [(Int, Int, Int)] -> Int -> Int
resolve1 points iters = foldl (*) 1 $ take 3 $ reverse $ sort $ map (length) circuits
  where
    circuits = build_circuits nearests $ map (Set.singleton) points
    len = length points
    nearests = take iters $ sortOn distance [(points !! i, points !! j) | i <- [0..len-1], j <- [i+1..len-1]]

distance :: ((Int, Int, Int), (Int, Int, Int)) -> Int
distance ((x1, x2, x3), (y1,y2,y3)) = a * a + b * b + c * c
  where
    a = x1 - y1
    b = x2 - y2
    c = x3 - y3

build_circuits :: [((Int,Int,Int),(Int,Int,Int))] -> [Set.Set (Int,Int,Int)] -> [Set.Set (Int,Int,Int)]
build_circuits [] s = s
build_circuits ((a,b):xs) sets = build_circuits xs sol
  where
    sol = (union_circuits:filter_a_b)
    filter_a = filter (not . Set.member a) sets
    filter_a_b = filter (not . Set.member b) filter_a
    s_a = head $ filter (Set.member a) sets
    s_b = head $ filter (Set.member b) sets
    union_circuits = Set.union s_a s_b

build_circuits2 :: [((Int,Int,Int),(Int,Int,Int))] -> [Set.Set (Int,Int,Int)] -> ((Int,Int,Int), (Int,Int,Int))
build_circuits2 [] s = ((0,0,0),(0,0,0))
build_circuits2 ((a,b):xs) sets = if (length $ filter (\x -> length x == 1) sets) == 1 then (search_next last_one xs) else build_circuits2 xs sol
  where
    sol = (union_circuits:filter_a_b)
    filter_a = filter (not . Set.member a) sets
    filter_a_b = filter (not . Set.member b) filter_a
    s_a = head $ filter (Set.member a) sets
    s_b = head $ filter (Set.member b) sets
    union_circuits = Set.union s_a s_b
    last_one = Set.elemAt 0 $ head $ filter (\x -> length x == 1) sets

search_next :: (Int,Int,Int) -> [((Int,Int,Int),(Int,Int,Int))] -> ((Int,Int,Int), (Int,Int,Int))
search_next a [] = (a,a)
search_next a ((b,c):xs)
  | a == b = (a,c)
  | a == c = (a,b)
  | otherwise = search_next a xs


resolve2 :: [(Int, Int, Int)] -> Int
resolve2 points = x1 * x2
  where
    (x1,_,_) = a
    (x2,_,_) = b
    (a,b) = build_circuits2 nearests $ map (Set.singleton) points
    len = length points
    nearests = sortOn distance [(points !! i, points !! j) | i <- [0..len-1], j <- [i+1..len-1]]
