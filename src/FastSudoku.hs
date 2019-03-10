module FastSudoku (
  readPuzzle,
  solvePuzzle,
  prettyShow,
  solve,
  Puzzle,
  Box(..)
) where

import Data.List ((\\), sortOn)

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

data Box = Box Int Int Int (Either [Int] Int)
instance Show Box where
  show (Box _ _ _ (Left ns)) = "_" 
  show (Box _ _ _ (Right n)) = show n
type Puzzle = [Box]

index :: Box -> Int
index (Box c r _ _) = c + houseSize * r

prettyShow :: Puzzle -> String
prettyShow = prettyShowH 0 0 . (sortOn index) where
  prettyShowH :: Int -> Int -> Puzzle -> String
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
readPuzzle = fixLefts . (readPuzzleH 0 0) where
  readPuzzleH :: Int -> Int -> String -> [Box]
  readPuzzleH _ _ [] = []
  readPuzzleH i j (c:s)
    | i == maxl = (parse maxl j c) : readPuzzleH 0 (j + 1) s
    | otherwise = (parse i j c)    : readPuzzleH (i + 1) j s
  numSet = [1..houseSize]
  parse :: Int -> Int -> Char -> Box
  parse i j c = case c of
    '_' -> (Box i j (calculateBlock i j) (Left numSet))
    c   -> (Box i j (calculateBlock i j) (Right (read . pure $ c)))
  fixLefts :: Puzzle -> Puzzle
  fixLefts puzzle = map fixLeftsH puzzle where
    fixLeftsH :: Box -> Box
    fixLeftsH b@(Box _ _ _ (Right _)) = b
    fixLeftsH (Box c r block (Left ns)) =
      (Box c r block (Left $ ns \\ (taken c r block puzzle)))
    taken :: Int -> Int -> Int -> Puzzle -> [Int]
    taken _ _ _ [] = []
    taken c r block ((Box i j bl (Right n)):bs) =
      if (c == i || r == j || block == bl)
        then n : rest
        else     rest
      where rest = taken c r block bs
    taken c r block (_:bs) = taken c r block bs

calculateBlock :: Int -> Int -> Int
calculateBlock i j = let
  row = floor $ (fromIntegral i) / (fromIntegral blockSize)
  col = floor $ (fromIntegral j) / (fromIntegral blockSize)
  in row + blockSize * col

guessBox :: Box -> [Box]
guessBox b@(Box _ _ _ (Right _)) = return b
guessBox (Box c r b (Left ns)) = do
  g <- ns
  return (Box c r b (Right g))

guess :: Puzzle -> [Puzzle]
guess p = do
  let box = head $ sortBoxes $ filter isUnsolved p
  newBox <- guessBox box
  return $ updatePuzzle p newBox
  where
    sortBoxes :: [Box] -> [Box]
    sortBoxes = sortOn numAvailable
    numAvailable :: Box -> Int
    numAvailable (Box _ _ _ (Left ns)) = length ns
    isUnsolved :: Box -> Bool
    isUnsolved (Box _ _ _ (Right _)) = False
    isUnsolved (Box _ _ _ (Left _))  = True 
    updatePuzzle :: Puzzle -> Box -> Puzzle
    updatePuzzle [] _ = []
    updatePuzzle (box@(Box i j block _):bs) newBox@(Box c r tarBlock (Right newVal))
      | i == c && j == r = newBox :updatePuzzle bs newBox
      | i == c || j == r || block == tarBlock = (case box of
        (Box _ _ _ (Right _)) -> box
        (Box c r b (Left ns)) -> Box c r b (Left (ns \\ [newVal]))
        ) :updatePuzzle bs newBox
      | otherwise        = box    :updatePuzzle bs newBox

solvePuzzle :: Puzzle -> [Puzzle]
solvePuzzle p = solvePuzzleH (numUnsolved p) p where
  solvePuzzleH :: Int -> Puzzle -> [Puzzle]
  solvePuzzleH numLeft puzzle
    | numLeft == 1 = guess puzzle
    | otherwise    = guess puzzle >>= solvePuzzleH (numLeft - 1)
  numUnsolved :: Puzzle -> Int
  numUnsolved = length . filter isUnsolved
  isUnsolved :: Box -> Bool
  isUnsolved (Box _ _ _ (Right _)) = False
  isUnsolved (Box _ _ _ (Left _))  = True 
