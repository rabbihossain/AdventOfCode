module AOC2021.Day05 where

import Data.List.Split (splitOn)
import Data.List.Unique (repeated, uniq, unique)

solveDay05 :: IO ()
solveDay05 = do
  input <- readFile "inputs/2021/Day05.input"
  let dataset = formatInputs $ map (splitOn " -> ") (lines input)
  print (partOne dataset, partTwo dataset)

convertTuple :: [b] -> (b, b)
convertTuple xy = (head xy, last xy)

generateCoordsWithStraightLines :: ((String, String), (String, String)) -> [(Int, Int)]
generateCoordsWithStraightLines ((x1, y1), (x2, y2))
  | x1 == x2 = [(read x1 :: Int, y) | y <- y1y2]
  | y1 == y2 = [(x, read y1 :: Int) | x <- x1x2]
  | otherwise = []
  where
    x1x2 = getRange (read x1 :: Int, read x2 :: Int)
    y1y2 = getRange (read y1 :: Int, read y2 :: Int)

generateCoordsWithDiagonalLines :: ((String, String), (String, String)) -> [(Int, Int)]
generateCoordsWithDiagonalLines ((x1, y1), (x2, y2))
  | x1 == x2 = [(read x1 :: Int, y) | y <- y1y2]
  | y1 == y2 = [(x, read y1 :: Int) | x <- x1x2]
  | otherwise = zip x1x2 y1y2
  where
    x1x2 = getRange (read x1 :: Int, read x2 :: Int)
    y1y2 = getRange (read y1 :: Int, read y2 :: Int)

getRange :: (Ord a, Enum a) => (a, a) -> [a]
getRange (x, y)
  | x < y = [x .. y]
  | x > y = reverse [y .. x]
  | otherwise = [x]

formatInputs :: [[String]] -> [((String, String), (String, String))]
formatInputs [] = []
formatInputs (d : ds) = (convertTuple $ head x, convertTuple $ last x) : formatInputs ds
  where
    x = map (splitOn ",") d

partOne :: [((String, String), (String, String))] -> Int
partOne = length . repeated . concatMap generateCoordsWithStraightLines

partTwo :: [((String, String), (String, String))] -> Int
partTwo = length . repeated . concatMap generateCoordsWithDiagonalLines