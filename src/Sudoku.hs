module Sudoku (
  readPuzzle,
  solvePuzzle,
  prettyShow,
  solve,
  Puzzle,
  Box(..)
) where

import Data.List ((\\))

solve :: String -> String
solve s = case (check s) of
  Just s  -> concat . fmap prettyShow . solvePuzzle $ readPuzzle s
  Nothing -> "Puzzle input was not " ++ show (houseSize ^ 2) ++ " chars."
  where
    check :: String -> Maybe String
    check s = if (length s /= houseSize ^ 2) then Nothing else Just s

blockSize = 3
houseSize = blockSize ^ 2
maxl = houseSize - 1

data Box = Box Int Int Int (Either () Int)
instance Show Box where
  show (Box _ _ _ (Left ())) = "_"
  show (Box _ _ _ (Right n)) = show n
type Puzzle = [Box]

prettyShow :: Puzzle -> String
prettyShow = prettyShowH 0 0 where
  prettyShowH _ _ [] = ""
  prettyShowH i j (b:bs)
    | i == maxl && j == maxl = show b
    | i == maxl && j `mod` blockSize == blockSize - 1 =
      show b ++ "\n\n" ++ prettyShowH 0 (j + 1) bs
    | i == maxl =
      show b ++ "\n" ++ prettyShowH 0 (j + 1) bs
    | i `mod` blockSize == blockSize - 1 =
      show b ++ "   " ++ prettyShowH (i + 1) j bs
    | otherwise =
      show b ++ " " ++ prettyShowH (i + 1) j bs


readPuzzle :: String -> Puzzle
readPuzzle = readPuzzleH 0 0 where
  readPuzzleH :: Int -> Int -> String -> [Box]
  readPuzzleH _ _ [] = []
  readPuzzleH i j (c:s)
    | i == maxl = (parse maxl j c) : readPuzzleH 0 (j + 1) s
    | otherwise = (parse i j c)    : readPuzzleH (i + 1) j s
  parse :: Int -> Int -> Char -> Box
  parse i j c = case c of
    '_' -> (Box i j (calculateBlock i j) (Left ()))
    c   -> (Box i j (calculateBlock i j) (Right (read . pure $ c)))

calculateBlock :: Int -> Int -> Int
calculateBlock i j = let
  row = floor $ (fromIntegral i) / (fromIntegral blockSize)
  col = floor $ (fromIntegral j) / (fromIntegral blockSize)
  in row + blockSize * col

possibleValsAt :: Int -> Int -> Puzzle -> [Int]
possibleValsAt _ _ [] = []
possibleValsAt i j puzzle = let
  numSet = [1..houseSize]
  blockNum = calculateBlock i j
  takenNums :: Puzzle -> [Int]
  takenNums [] = []
  takenNums ((Box c r b (Right n)):bs) =
    if (c == i || r == j || b == blockNum)
      then n : rest
      else rest
    where rest = takenNums bs
  takenNums (_:bs) = takenNums bs

  in numSet \\ (takenNums puzzle)

guessBox :: Puzzle -> Box -> [Box]
guessBox _ b@(Box c r _ (Right _)) = return b
guessBox p (Box c r block (Left ())) = do
  g <- possibleValsAt c r p
  return (Box c r block (Right g))

guess :: Int -> Int -> Puzzle -> [Puzzle]
guess i j puzzle = do
  let box = findBox i j puzzle
  newBox <- guessBox puzzle box
  return $ updatePuzzle puzzle newBox
  where
    findBox :: Int -> Int -> Puzzle -> Box
    findBox i j (b@(Box c r _ _):bs)
      | i == c && j == r = b
      | otherwise        = findBox i j bs
    updatePuzzle :: Puzzle -> Box -> Puzzle
    updatePuzzle (box@(Box i j _ _):bs) newBox@(Box c r _ _)
      | i == c && j == r = newBox :bs
      | otherwise        = box    :updatePuzzle bs newBox

solvePuzzle :: Puzzle -> [Puzzle]
solvePuzzle = solvePuzzleH 0 0 where
  solvePuzzleH :: Int -> Int -> Puzzle -> [Puzzle]
  solvePuzzleH i j puzzle
    | i == maxl && j == maxl = guess i j puzzle
    | i == maxl              = guess i j puzzle >>= solvePuzzleH 0 (j + 1)
    | otherwise              = guess i j puzzle >>= solvePuzzleH (i + 1) j
