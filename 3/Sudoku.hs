module Sudoku where


-- ========================== lab 3A =================

-- Sudoku datatype
data Sudoku = Sudoku { rows :: [[Maybe Int]]}
     deriving (Show)    

-- A1: Returns an empty Sudoku
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku {rows = replicate 9 (replicate 9 Nothing)}
