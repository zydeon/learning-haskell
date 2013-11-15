module Sudoku where


-- ========================== lab 3A =================

-- Sudoku datatype
data Sudoku = Sudoku { rows :: [[Maybe Int]]}
     deriving (Show)    

-- A1: Returns an empty Sudoku
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
checkSize (l1:[])                     = (length l1 == 9)
checkSize (l1:ls) | (length l1 == 9)  = checkSize ls
                  | otherwise         = False  

-- Checks the elemens of each list
checkLists :: [[Maybe Int]] -> Bool
checkLists (l1 : [])                = isInRange l1
checkLists (l1 : ls) | isInRange l1 = checkLists ls
                     | otherwise    = False 

-- Checks the range of elements
isInRange :: [Maybe Int] -> Bool
isInRange (Just a : [])                     = (a<10) && (a>0)
isInRange (Nothing : [])                    = True  
isInRange (Just a : nums) | (a<10) && (a>0) = isInRange nums
                          | otherwise       = False
isInrange (Nothing : nums)                  = isInRange nums
