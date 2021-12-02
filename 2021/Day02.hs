{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day02 where

main :: IO ()
main = do
  input <- readFile "Day02.input"
  let moves = [(x, y) | i <- lines input, let s = span (/= ' ') i, let x = fst s, let y = drop 1 $ snd s]
  print (partOne moves, partTwo moves)

goThroughMoves :: [(String, String)] -> (Int, Int)
goThroughMoves = foldl calculatePosition (0, 0)

goThroughMovesAndAims :: [(String, String)] -> [Int]
goThroughMovesAndAims = foldl calculatePositionWithAim [0, 0, 0]

calculatePositionWithAim :: [Int] -> ([Char], String) -> [Int]
calculatePositionWithAim [h, d, a] (direction, value)
  | direction == "down" = [h, d, a + read value]
  | direction == "up" = [h, d, a - read value]
  | otherwise = [h + read value, d + (read value * a), a]

calculatePosition :: (Int, Int) -> ([Char], String) -> (Int, Int)
calculatePosition (h, d) (direction, value)
  | direction == "down" = (h, d + read value)
  | direction == "up" = (h, d - read value)
  | otherwise = (h + read value, d)

partOne :: [(String, String)] -> Int
partOne moves = uncurry (*) calculatedMoves
  where
    calculatedMoves = goThroughMoves moves

partTwo :: [(String, String)] -> Int
partTwo moves = head calculatedMoves * calculatedMoves !! 1
  where
    calculatedMoves = goThroughMovesAndAims moves