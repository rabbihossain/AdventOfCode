module AOC2021.Day04 where

import AOC2021.Day03 (zipMany)
import Data.List.Split (chunksOf, splitOn)

solveDay04 :: IO ()
solveDay04 = do
  input <- readFile "inputs/2021/Day04.input"
  let dataset = lines input
  let inputBoards = map sanitizeBoard $ chunksOf 5 [b | b <- tail dataset, b /= ""]
  let draws = sanitizeDraws $ head dataset
  let solution = boardsRunner draws inputBoards
  print (partOneNTwo . getWinningBoard $ solution, partOneNTwo . getLosingBoard $ solution)

type Boards = [Board]

type Board = [Row]

type Row = [String]

type Value = String

data Result a b c = MkResult
  { board :: Board,
    solved :: Board,
    draws :: [Value]
  }
  deriving (Show)

getSolved :: Result a b c -> Board
getSolved = solved

getBoard :: Result a b c -> Board
getBoard = board

getLastDraws :: Result a b c -> [Value]
getLastDraws = draws

emptyResult :: Result a b c
emptyResult = MkResult {board = [], solved = [], draws = []}

sanitizeDraws :: String -> Row
sanitizeDraws = splitOn ","

sanitizeBoard :: Row -> Board
sanitizeBoard = map words

markRow :: Row -> Value -> Row
markRow rs d = map (`markNumber` d) rs

markNumber :: Value -> Value -> Value
markNumber v d
  | v == d = ""
  | otherwise = v

markBoard :: Board -> Value -> Board
markBoard rs d = map (`markRow` d) rs

isBoardsSolved :: Board -> Bool
isBoardsSolved [] = False
isBoardsSolved (row : rows)
  | row == replicate 5 "" = True
  | otherwise = isBoardsSolved rows

boardFinder :: Boards -> Boards
boardFinder = filter (\x -> isBoardsSolved x || isBoardsSolved (zipMany x))

boardSolver :: [Value] -> Board -> [Value] -> Board -> Result a b c
boardSolver [] board ld ogBoard =
  MkResult
    { board = ogBoard,
      solved = board,
      draws = ld
    }
boardSolver (d : ds) board ld ogBoard
  | isBoardsSolved board || isBoardsSolved (zipMany board) =
    MkResult
      { board = ogBoard,
        solved = board,
        draws = ld
      }
  | otherwise = boardSolver ds (markBoard board d) (d : ld) ogBoard

filterBoards :: [Result a b c] -> [Result a b c]
filterBoards = filter (\x -> getBoard x /= getSolved x)

boardsRunner :: [Value] -> [Board] -> [Result a b c]
boardsRunner ds bs = filterBoards $ map (\b -> boardSolver ds b [] b) bs

getWinningBoard :: [Result a b c] -> Result a b c
getWinningBoard [] = emptyResult
getWinningBoard (b : bs) = getWinningBoard' bs b
  where
    getWinningBoard' [] lb = lb
    getWinningBoard' (x : xs) lb
      | length (getLastDraws x) < length (getLastDraws lb) = getWinningBoard' xs x
      | otherwise = getWinningBoard' xs lb

getLosingBoard :: [Result a b c] -> Result a b c
getLosingBoard [] = emptyResult
getLosingBoard (b : bs) = getLosingBoard' bs b
  where
    getLosingBoard' [] lb = lb
    getLosingBoard' (x : xs) lb
      | length (getLastDraws x) > length (getLastDraws lb) = getLosingBoard' xs x
      | otherwise = getLosingBoard' xs lb

calculateBoard :: (Num a, Read a) => Board -> a
calculateBoard board = sum [read v | r <- board, v <- r, v /= ""]

partOneNTwo :: (Num a1, Read a1) => Result a2 b c -> a1
partOneNTwo board = calculateBoard (getSolved board) * read lastDraw
  where
    lastDraw = head (getLastDraws board)