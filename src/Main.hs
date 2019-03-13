module Main (main) where

import System.Environment
import FastSudoku hiding (solve, Box)
import Data.Char (toLower)

main = do
  args <- getArgs
  case args of
    (filename:as) -> printFromFile filename
    otherwise -> runUserIO

runUserIO :: IO ()
runUserIO = do
  putStrLn "File or String?"
  fos <- getLine >>= return . fmap toLower
  case fos of
    "file"   -> return ()
    "string" -> return ()
    _        -> do
      putStrLn "I can't understand that. Try again:"
      main
  selectOutputStep fos

printFromFile :: String -> IO ()
printFromFile filename = do
  puzzles <- puzzlesFromFile filename
  solvePrintAll puzzles

selectOutputStep :: String -> IO ()
selectOutputStep fos = do
  putStrLn "Output to File or stdout?"
  outChoice <- getLine >>= return . fmap toLower
  case outChoice of
    "file"   -> return ()
    "stdout" -> return ()
    _        -> do
      putStrLn "I can't understand that."
      selectOutputStep fos
  ioDriver fos outChoice

ioDriver :: String -> String -> IO ()
ioDriver inChoice outChoice = do
  puzzles <- case inChoice of
    "file"   -> inFile
    "string" -> inString
  solveOutputDriver outChoice puzzles

inFile :: IO [String]
inFile = do
  putStrLn "What's the input filename?"
  filename <- getLine
  puzzlesFromFile filename

puzzlesFromFile :: String -> IO [String]
puzzlesFromFile filename = do
  fileContents <- readFile filename >>= return . lines
  if (fileContents == [])
    then do {putStrLn "File is empty."; return []}
    else if (head fileContents /= "#spf1.0")
      then do
        putStrLn "File does not contain Sudoku Puzzle Format 1.0 header."
        return []
      else return $ tail fileContents


inString :: IO [String]
inString = do
  putStrLn "Enter a puzzle string using the #spf1.0 formatting standard:"
  puzzle <- getLine
  return . return $ puzzle

solveOutputDriver :: String -> [String] -> IO ()
solveOutputDriver outChoice puzzles = case outChoice of
  "stdout" -> solvePrintAll puzzles
  "file"   -> do
    putStrLn "What's the ouput filename?"
    filename <- getLine
    writeFile filename $ concat . fmap solveToString $ puzzles
    putStrLn "Done."
  
-- SOLVERS TO OUTPUT IO ACTIONS AND STRINGS
solvePrintAll :: [String] -> IO ()
solvePrintAll [] = return ()
solvePrintAll (p:ps) = solveAndPrint p >> solvePrintAll ps

solveAndPrint :: String -> IO ()
solveAndPrint s = do
  let puzzle = readPuzzle s
  putStr $ "-----------------------NEW PUZZLE------------------------\n" ++
    prettyShow puzzle
  putStrLn $ "-------------------------SOLVING-------------------------"
  showSolns 1 (solvePuzzle puzzle)
  where
    showSolns :: Int -> [Puzzle] -> IO ()
    showSolns _ []       = putStrLn $ "No Solutions." ++
      "\n---------------------------------------------------------"
    showSolns i (s:[]) = putStrLn $ prettyShow s ++
      "--------------------------------------------------------- " ++ show i
    showSolns i (s:ss) = do
      putStrLn $ prettyShow s ++
        "--------------------------------------------------------- " ++ show i
      showSolns (i + 1) ss

solveToString :: String -> String
solveToString s =
  let puzzle = readPuzzle s in
  "-----------------------NEW PUZZLE------------------------\n" ++
    prettyShow puzzle ++
    "------------------------SOLUTIONS------------------------\n" ++
    stringifySolns 1 (solvePuzzle puzzle)
  where
    stringifySolns :: Int -> [Puzzle] -> String
    stringifySolns _ [] = "No Solutions." ++
      "\n---------------------------------------------------------\n\n"
    stringifySolns i (s:[]) = prettyShow s ++
      "--------------------------------------------------------- " ++
      show i ++ "\n\n"
    stringifySolns i (s:ss) = prettyShow s ++
      "--------------------------------------------------------- " ++
        show i ++ "\n" ++ stringifySolns (i + 1) ss
