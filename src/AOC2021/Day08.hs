module AOC2021.Day08 where

import Data.List.Split (splitOn)

solveDay08 :: IO ()
solveDay08 = do
  input <- readFile "./inputs/2021/Day08.input"
  let dataset = [(words . head $ splitOn "|" x, words . last $ splitOn "|" x) | x <- lines input]
  print (partOne dataset, partTwo dataset)

guessThatDigit :: [String] -> [Char] -> Char
guessThatDigit x y
  | length y == 2 = '1'
  | length y == 3 = '7'
  | length y == 4 = '4'
  | length y == 5 = decideTwoThreeFive x y
  | length y == 6 = decideZeroSixNine x y
  | otherwise = '8'

decideZeroSixNine :: (Eq a, Foldable t) => [t a] -> [a] -> Char
decideZeroSixNine xs d
  | length (filter (`elem` four) d) == 4 && length (filter (`elem` one) d) == 2 = '9'
  | length (filter (`elem` four) d) == 3 && length (filter (`elem` one) d) == 2 = '0'
  | otherwise = '6'
  where
    one = head [x | x <- xs, length x == 2]
    four = head [x | x <- xs, length x == 4]

decideTwoThreeFive :: (Eq a, Foldable t) => [t a] -> [a] -> Char
decideTwoThreeFive xs d
  | length (filter (`elem` four) d) == 3 && length (filter (`elem` one) d) == 2 = '3'
  | length (filter (`elem` four) d) == 3 && length (filter (`elem` one) d) == 1 = '5'
  | otherwise = '2'
  where
    one = head [x | x <- xs, length x == 2]
    four = head [x | x <- xs, length x == 4]

findEasyNumbers :: ([String], [[Char]]) -> [Int]
findEasyNumbers (firstHalf, lastHalf) = [read x :: Int | x <- lastHalf, guessThatDigit firstHalf x == '1' || guessThatDigit firstHalf x == '4' || guessThatDigit firstHalf x == '7' || guessThatDigit firstHalf x == '8']

findAllNumbers :: ([String], [String]) -> String
findAllNumbers (firstHalf, lastHalf) = map (guessThatDigit firstHalf) lastHalf

partOne :: Foldable t => t ([String], [String]) -> Int
partOne dataset = length $ concatMap findEasyNumbers dataset

partTwo :: [([String], [String])] -> Int
partTwo dataset = sum $ map ((\x -> read x :: Int) . findAllNumbers) dataset