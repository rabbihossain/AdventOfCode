module AOC2021.Day03 where

solveDay03 :: IO ()
solveDay03 = do
  input <- readFile "inputs/2021/Day03.input"
  let dataset = lines input
  let dataColumns = zipMany dataset
  print (partOne dataColumns, partTwo dataColumns dataset)

commons :: Ord b => [b] -> (b, b)
commons input = (snd . maximum $ cm input, snd . minimum $ cm input)
  where
    cm [] = []
    cm (x : xs) = (length [c | c <- xs, c == x] + 1, x) : cm [u | u <- xs, u /= x]

zipMany :: [[a]] -> [[a]]
zipMany input
  | null $ head input = []
  | otherwise = [head x | x <- input] : zipMany [tail x | x <- input]

binToDec :: String -> Integer
binToDec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
  where
    c2i c = if c == '0' then 0 else 1

powerConsumption :: [String] -> (String, String)
powerConsumption columns = (gamma, epsilon)
  where
    (gamma, epsilon) = unzip . map commons $ columns

o2Rating :: [[Char]] -> Int -> [Char]
o2Rating dataset totalCount = counter 0 totalCount dataset
  where
    counter x total dataset
      | x < total = counter (x + 1) total $ o2Picker x dataset
      | otherwise = head dataset

o2Picker :: Int -> [[Char]] -> [[Char]]
o2Picker index dt
  | length dt == 1 = dt
  | length q1 + length q0 == 1 && length q1 > length q0 = q1
  | length q1 + length q0 == 1 && length q0 > length q1 = q0
  | length q1 > length q0 = q1
  | length q1 < length q0 = q0
  | otherwise = q1
  where
    q1 = filter (\x -> x !! index == '1') dt
    q0 = filter (\x -> x !! index == '0') dt

co2Rating :: [[Char]] -> Int -> [Char]
co2Rating dataset totalCount = counter 0 totalCount dataset
  where
    counter x total dataset
      | x < total = counter (x + 1) total $ co2Picker x dataset
      | otherwise = head dataset

co2Picker :: Int -> [[Char]] -> [[Char]]
co2Picker index dt
  | length dt == 1 = dt
  | length q1 + length q0 == 1 && length q1 > length q0 = q1
  | length q1 + length q0 == 1 && length q0 > length q1 = q0
  | length q1 > length q0 = q0
  | length q1 < length q0 = q1
  | otherwise = q0
  where
    q1 = filter (\x -> x !! index == '1') dt
    q0 = filter (\x -> x !! index == '0') dt

partOne :: [String] -> Integer
partOne columns = (binToDec . fst $ powerConsumption columns) * (binToDec . snd $ powerConsumption columns)

partTwo :: [String] -> [[Char]] -> Integer
partTwo columns dataset = binToDec calculatedO2 * binToDec calculatedCO2
  where
    (calculatedO2, calculatedCO2) = (o2Rating dataset $ length gamma, co2Rating dataset $ length epsilon)
    (gamma, epsilon) = powerConsumption columns :: (String, String)