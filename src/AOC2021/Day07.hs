module AOC2021.Day07 where

import Data.List (foldl', sort)
import Data.List.Split (splitOn)

solveDay07 :: IO ()
solveDay07 = do
  input <- readFile "./inputs/2021/Day07.input"
  let dataset = sort . map (\x -> read x :: Int) $ splitOn "," input
  print (partOne dataset, partTwo dataset)

partOne :: (Num a, Ord a, Integral a) => [a] -> a
partOne crabs = foldl (\a v -> a + abs (v - mdnOfCrabs)) 0 crabs
  where
    mdnOfCrabs = mdn crabs

partTwo :: Foldable t => t Int -> Int
partTwo crabs = foldl (\a v -> a + distance v avgOfCrabs) 0 crabs
  where
    distance p1 p2 = (abs (p1 - p2) * (abs (p1 - p2) + 1)) `div` 2
    avgOfCrabs = avg' crabs

mdn :: Integral a => [a] -> a
mdn [] = 0
mdn [x] = x
mdn [x, y] = sum [x, y] `div` 2
mdn xs = mdn . init . tail $ sort xs

-- adding a +1 to avoid a corner case when sum is odd
avg' :: Foldable t => t Int -> Int
avg' x
  | even $ sum x = sum x `div` length x
  | otherwise = (sum x + 1) `div` length x
