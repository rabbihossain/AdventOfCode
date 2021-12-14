module AOC2021.Day09 where

import Data.List (foldl, nub, sort)
import Data.Map (Map, empty, findWithDefault, foldlWithKey, fromList)

solveDay09 :: IO ()
solveDay09 = do
  input <- readFile "./inputs/2021/Day09.input"
  let dataset = fromList $ zip [0 .. (length $ lines input)] [fromList $ zip [0 .. length x] (map (\i -> read [i] :: Int) x) | x <- lines input]
  let basins = concatMap (\xs -> [(x, y, z) | (x, y, z) <- xs, z /= 9]) $ getLowPoints dataset
  print (partOne dataset, partTwo basins dataset)

findMap :: Ord k1 => k1 -> Map k1 (Map k2 a) -> Map k2 a
findMap = findWithDefault empty

findValue :: (Ord k, Num a) => a -> k -> Map k a -> a
findValue value = findWithDefault 9

basinValue :: (a, b, c) -> c
basinValue (_, _, v) = v

findLowPoints :: (Ord c, Num k1, Ord k1, Num k2, Ord k2, Num c) => k1 -> k2 -> c -> Map k1 (Map k2 c) -> (k1, k2, c)
findLowPoints rowKey valueKey currentValue dataset
  | currentValue < getTop
      && currentValue < getBottom
      && currentValue < getRight
      && currentValue < getLeft =
    (rowKey, valueKey, currentValue)
  | otherwise = (rowKey, valueKey, 9)
  where
    getTop = findValue currentValue valueKey topRow
    getBottom = findValue currentValue valueKey bottomRow
    getRight = findValue currentValue (valueKey + 1) currentRow
    getLeft = findValue currentValue (valueKey - 1) currentRow
    currentRow = findMap rowKey dataset
    topRow = findMap (rowKey - 1) dataset
    bottomRow = findMap (rowKey + 1) dataset

findNextNeighbours :: (Foldable t, Eq c, Ord a, Ord b, Num c, Num a, Num b) => (a, b, c) -> Map a (Map b c) -> t (a, b, c) -> [(a, b, c)]
findNextNeighbours (rowKey, valueKey, currentValue) dataset visited = [x | x <- foundNeighbours, x `notElem` visited, basinValue x /= 9]
  where
    foundNeighbours = [(rowKey - 1, valueKey, getTop), (rowKey, valueKey + 1, getRight), (rowKey + 1, valueKey, getBottom), (rowKey, valueKey -1, getLeft)]
    getTop = findValue currentValue valueKey topRow
    getBottom = findValue currentValue valueKey bottomRow
    getRight = findValue currentValue (valueKey + 1) currentRow
    getLeft = findValue currentValue (valueKey - 1) currentRow
    currentRow = findMap rowKey dataset
    topRow = findMap (rowKey - 1) dataset
    bottomRow = findMap (rowKey + 1) dataset

goThroughPoints :: (Ord c, Ord k1, Ord k2, Num k1, Num k2, Num c) => k1 -> Map k2 c -> Map k1 (Map k2 c) -> [(k1, k2, c)]
goThroughPoints rowKey row dataset = foldlWithKey (\a k v -> findLowPoints rowKey k v dataset : a) [] row

getLowPoints :: (Ord c, Ord k1, Ord k2, Num k1, Num k2, Num c) => Map k1 (Map k2 c) -> [[(k1, k2, c)]]
getLowPoints dataset = foldlWithKey (\a k v -> goThroughPoints k v dataset : a) [] dataset

searchBasin :: (Ord a3, Ord a2, Num a3, Num a2) => (a3, a2, a3) -> Map a3 (Map a2 a3) -> [(a3, a2, a3)]
searchBasin basin dataset = looper (findNextNeighbours basin dataset [basin]) [basin]
  where
    looper nbs vbs
      | not (null nbs) = looper (nub $ concatMap (\b -> findNextNeighbours b dataset (nub vbs ++ nbs)) nbs) (nub vbs ++ nbs)
      | otherwise = vbs

basinExplorer :: (Ord a3, Ord a2, Num a3, Num a2) => [(a3, a2, a3)] -> Map a3 (Map a2 a3) -> [[(a3, a2, a3)]]
basinExplorer [] _ = []
basinExplorer (b : bs) dataset = searchBasin b dataset : basinExplorer bs dataset

partOne :: (Ord a1, Ord a2, Ord b, Num a1, Num a2, Num b) => Map a2 (Map b a1) -> a1
partOne dataset = sum $ concatMap (\xs -> [z + 1 | (x, y, z) <- xs, z /= 9]) $ getLowPoints dataset

partTwo :: (Ord a3, Ord a2, Num a3, Num a2) => [(a3, a2, a3)] -> Map a3 (Map a2 a3) -> Int
partTwo basins dataset = product $ take 3 $ reverse $ sort [length x | x <- basinExplorer basins dataset]
