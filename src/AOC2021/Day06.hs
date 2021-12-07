module AOC2021.Day06 where

import Data.List (group)
import Data.List.Split (splitOn)
import Data.Map (Map, findWithDefault, foldl', fromList, update)
import Data.Maybe (fromMaybe)

solveDay06 :: IO ()
solveDay06 = do
  input <- readFile "./inputs/2021/Day06.input"
  let dataset = map (\x -> read x :: Int) $ splitOn "," input
  let countedDataset = [(y, length a) | y <- [0 .. 8], a <- group [x | x <- dataset, x == y]]
  let dayZeroFishes = foldl (\a (k, v) -> update (addToValue v) k a) initialMap countedDataset
  print (partOne dayZeroFishes, partTwo dayZeroFishes)

initialMap :: Map Int Int
initialMap = fromList [(x :: Int, 0) | x <- [0 .. 8]]

passDays :: (Ord t, Num t) => t -> Map Int Int -> Map Int Int
passDays dayCount = passDays' 0
  where
    passDays' startingDay fishMap'
      | startingDay < dayCount = passDays' (startingDay + 1) (growFish fishMap')
      | otherwise = fishMap'

growFish :: Map Int Int -> Map Int Int
growFish map = growFish' 8 map initialMap
  where
    growFish' curKey inputMap mapTogen
      | curKey == 0 = update (addToValue currentVal) 8 $ update (addToValue currentVal) 6 mapTogen
      | otherwise = growFish' (curKey - 1) inputMap $ update (addToValue currentVal) (curKey - 1) mapTogen
      where
        currentVal = findValue curKey inputMap

findValue :: (Ord k, Num a) => k -> Map k a -> a
findValue = findWithDefault 0

addToValue :: Int -> Int -> Maybe Int
addToValue x y = Just (y + x)

partOne :: Map Int Int -> Int
partOne fishMap = foldl' (+) 1 $ passDays 80 fishMap

partTwo :: Map Int Int -> Int
partTwo fishMap = foldl' (+) 1 $ passDays 256 fishMap