module Day01 where

main :: IO ()
main = do
  input <- readFile "Day01.input"
  let moves = [read x | x <- lines input]
  print $ solution moves

increasedItems :: Ord a => [a] -> Int
increasedItems xs = length . filter id $ zipWith (<) xs (drop 1 xs)

slidedItems :: Num a => [a] -> [a]
slidedItems xs = zipWith3 (\x y z -> x + y + z) xs (drop 1 xs) (drop 2 xs)

-- solution function returns -> (Part1 Count, Part2 Count)
solution :: (Ord a, Num a) => [a] -> (Int, Int)
solution input = (increasedItems input, increasedItems $ slidedItems input)