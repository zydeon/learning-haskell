module Sudoku where

import Test.QuickCheck

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


hasNoth :: [Maybe Int] -> Bool
hasNoth list = and [ False | a <- list , (a == Nothing) ]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku = undefined

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined

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
