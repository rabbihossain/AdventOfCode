module Day02 where

main :: IO ()
main = do
  input <- readFile "Day02.input"
  let moves = [(x, y) | i <- lines input, let s = span (/= ' ') i, let x = fst s, let y = drop 1 $ snd s]
  print (partOne moves, partTwo moves)

goThroughMoves :: [(String, String)] -> (Int, Int)
goThroughMoves = foldl calculatePosition (0, 0)

goThroughMovesAndAims :: [([Char], String)] -> (Int, Int, Int)
goThroughMovesAndAims = foldl calculatePositionWithAim (0, 0, 0)

calculatePosition :: (Int, Int) -> ([Char], String) -> (Int, Int)
calculatePosition (h, d) (direction, value)
  | direction == "down" = (h, d + read value)
  | direction == "up" = (h, d - read value)
  | otherwise = (h + read value, d)

calculatePositionWithAim :: (Num a1, Num a2, Read a1, Read a2) => (a2, a1, a1) -> ([Char], String) -> (a2, a1, a1)
calculatePositionWithAim (h, d, a) (direction, value)
  | direction == "down" = (h, d, a + read value)
  | direction == "up" = (h, d, a - read value)
  | otherwise = (h + read value, d + (read value * a), a)

partOne :: [(String, String)] -> Int
partOne moves = uncurry (*) calculatedMoves
  where
    calculatedMoves = goThroughMoves moves

partTwo :: [([Char], String)] -> Int
partTwo moves = calculatedH * calculatedD
  where
    (calculatedH, calculatedD, calculatedA) = goThroughMovesAndAims moves