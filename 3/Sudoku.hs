
module Sudoku where

import Test.QuickCheck
import System.IO
import Data.Char

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )
   


-- A1: allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku {rows = replicate 9 (replicate 9 Nothing)}

-- A2: Checks the Sudoku conditions
isSudoku :: Sudoku -> Bool
isSudoku Sudoku {rows = list}
                | (length list == 9) && ( checkSize list &&
                     checkLists list) =  True

                | otherwise   = False 

-- Checks the size of each list
checkSize :: [[Maybe Int]] -> Bool
checkSize list = and [(length l == 9) | l <- list] 


-- Checks the elemens of each list
checkLists :: [[Maybe Int]] -> Bool
checkLists list = and [ isInRange a | a <- list]


-- Checks the range of elements
isInRange :: [Maybe Int] -> Bool
isInRange (Just a : [])                     = (a<10) && (a>0)
isInRange (Nothing : [])                    = True  
isInRange (Just a : nums) | (a<10) && (a>0) = isInRange nums
                          | otherwise       = False
isInrange (Nothing : nums)                  = isInRange nums



-- A3: isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved Sudoku {rows = list} = and [hasNoth a | a <- list]
  where 
    hasNoth :: [Maybe Int] -> Bool
    hasNoth list = and [ False | a <- list , (a == Nothing) ]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = mapM_ printCell (rows s)
  where 
    -- prints out each row of the Sudoku
    printCell :: [Maybe Int] -> IO ()
    printCell (c:[]) |c == Nothing = putStrLn "."
    printCell (c:[]) |otherwise    = putStrLn $ show a
                    where Just a = c
    printCell (c:cs) |c == Nothing = do
                                     putStr "."
                                     printCell cs
                     |otherwise    = do
                                     putStr $ show a
                                     printCell cs
                                      where Just a = c  


-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
          res <- openFile path ReadMode
          content <- hGetContents res
          if null content
          then do
           hClose res
           putStrLn "No Sudoku to read from file!\nAn empty sudoku is returned:"
           return allBlankSudoku
          else do              
             return $toSud $toLists $lines content
              where
                toSud :: [[Maybe Int]] -> Sudoku
                toSud ls = Sudoku {rows = ls}

                toLists :: [String] -> [[Maybe Int]]
                toLists (st:[])  = [toMaybe st]
                toLists (st:sts) = [toMaybe st] ++ toLists sts

                toMaybe :: String -> [Maybe Int]
                toMaybe (c:[]) |c == '.'  = [Nothing]
                               |otherwise = [Just (digitToInt c)]
                toMaybe (c:cs) |c == '.'  = [Nothing] ++ toMaybe cs
                               |otherwise = [Just (digitToInt c)] ++ toMaybe cs

-- To test!
main = do 
       r <-(readSudoku "C:/Users/Mozhan/Desktop/sudokus/hard40.sud")
       printSudoku r
-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------
