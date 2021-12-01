module Day01 where

increaseDecrease :: Ord a => [a] -> [Bool]
increaseDecrease [] = []
increaseDecrease [_] = []
increaseDecrease (x : y : xs)
  | x < y = True : increaseDecrease (y : xs)
  | otherwise = False : increaseDecrease (y : xs)

increasedCount :: [Bool] -> Int
increasedCount = length . filter id

slidedItems :: Num a => [a] -> [a]
slidedItems [] = []
slidedItems [_] = []
slidedItems [_, _] = []
slidedItems (x : y : z : xs) = (x + y + z) : slidedItems (y : z : xs)

partOne :: (Ord a, Num a) => [a] -> Int
partOne = increasedCount . increaseDecrease

partTwo :: (Ord a, Num a) => [a] -> Int
partTwo = increasedCount . increaseDecrease . slidedItems

solution :: (Ord a, Num a) => [a] -> (Int, Int)
solution input = (partOne input, partTwo input)