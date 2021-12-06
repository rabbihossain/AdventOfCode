module AOC2021.Day02 where

solveDay02 :: IO ()
solveDay02 = do
  input <- readFile "inputs/2021/Day02.input"
  let moves = [(x, y) | i <- lines input, let s = span (/= ' ') i, let x = fst s, let y = drop 1 $ snd s]
  print (partOne moves, partTwo moves)

goThroughMoves :: (Integer, Integer, Integer, Integer) -> ([Char], String) -> (Integer, Integer, Integer, Integer)
goThroughMoves = calculatePosition
  where
    calculatePosition (forward, depthWithAim, depth, aim) (direction, movement)
      | direction == "down" = (forward, depthWithAim, depth + read movement, aim + read movement)
      | direction == "up" = (forward, depthWithAim, depth - read movement, aim - read movement)
      | otherwise = (forward + read movement, depthWithAim + (read movement * aim), depth, aim)

partOne :: Foldable t => t ([Char], String) -> Integer
partOne moves = forward * depth
  where
    (forward, _, depth, _) = foldl goThroughMoves (0, 0, 0, 0) moves

partTwo :: Foldable t => t ([Char], String) -> Integer
partTwo moves = forward * depthWithAim
  where
    (forward, depthWithAim, _, _) = foldl goThroughMoves (0, 0, 0, 0) moves