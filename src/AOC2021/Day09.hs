module AOC2021.Day09 where

import Data.Map (Map, empty, findWithDefault, foldlWithKey, fromList)

solveDay09 :: IO ()
solveDay09 = do
  input <- readFile "./inputs/2021/Day09.input"
  let dataset = fromList $ zip [0 .. (length $ lines input)] [fromList $ zip [0 .. length x] (map (\i -> read [i] :: Int) x) | x <- lines input]
  print $ partOne dataset

partOne :: (Ord a1, Ord a2, Ord b, Num a1, Num a2, Num b) => Map a2 (Map b a1) -> a1
partOne dataset = sum $ concatMap (\xs -> [z + 1 | (x, y, z) <- xs, z /= -1]) $ getLowPoints dataset

getLowPoints :: (Ord c, Ord k1, Ord k2, Num k1, Num k2, Num c) => Map k1 (Map k2 c) -> [[(k1, k2, c)]]
getLowPoints dataset = foldlWithKey (\a k v -> goThroughPoints k v dataset : a) [] dataset

goThroughPoints :: (Ord c, Ord k1, Ord k2, Num k1, Num k2, Num c) => k1 -> Map k2 c -> Map k1 (Map k2 c) -> [(k1, k2, c)]
goThroughPoints rowKey row dataset = foldlWithKey (\a k v -> findLowPoints rowKey k v dataset : a) [] row

findLowPoints :: (Ord c, Num k1, Ord k1, Num k2, Ord k2, Num c) => k1 -> k2 -> c -> Map k1 (Map k2 c) -> (k1, k2, c)
findLowPoints rowKey valueKey currentValue dataset
  | currentValue < getTop
      && currentValue < getBottom
      && currentValue < getRight
      && currentValue < getLeft =
    (rowKey, valueKey, currentValue)
  | otherwise = (rowKey, valueKey, -1)
  where
    getTop = findValue currentValue valueKey topRow
    getBottom = findValue currentValue valueKey bottomRow
    getRight = findValue currentValue (valueKey + 1) currentRow
    getLeft = findValue currentValue (valueKey - 1) currentRow
    currentRow = findMap rowKey dataset
    topRow = findMap (rowKey - 1) dataset
    bottomRow = findMap (rowKey + 1) dataset

findMap :: Ord k1 => k1 -> Map k1 (Map k2 a) -> Map k2 a
findMap = findWithDefault empty

findValue :: (Ord k, Num a) => a -> k -> Map k a -> a
findValue value = findWithDefault 9