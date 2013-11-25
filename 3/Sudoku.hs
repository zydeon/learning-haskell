module Sudoku where

import Test.QuickCheck
import System.IO
import Data.Char
import Data.List

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )
   

-- A1: allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku {rows = replicate 9 (replicate 9 Nothing)}

-- A2: checks the sudoku conditions
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku list) = (length list == 9) && (checkSize list && checkLists list)

-- Checks the size of each list
checkSize :: [[Maybe Int]] -> Bool
checkSize list = and [(length l == 9) | l <- list] 

-- Checks the elemens of each list
checkLists :: [[Maybe Int]] -> Bool
checkLists list = and [inRange a | a <- list]
  where
    -- Checks the range of elements
    inRange :: [Maybe Int] -> Bool
    inRange mbs = and $ map checkRange mbs
        where 
          checkRange :: Maybe Int -> Bool
          checkRange (Just a) = (a<10) && (a>0)
          checkRange _        = True 


-- A3: isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved (Sudoku list) = and [hasNoth a | a <- list]
 
hasNoth :: [Maybe Int] -> Bool
hasNoth list = and [False | a <- list , (a == Nothing)]

-------------------------------------------------------------------------

-- B1: printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = mapM_ printCell (rows s)
  where 
    -- prints out each row of the Sudoku
    printCell :: [Maybe Int] -> IO ()
    printCell []                   = putStrLn ""
    printCell (c:cs) |c == Nothing = do
                                     putStr "."
                                     printCell cs
                     |otherwise    = do
                                     putStr $ show a
                                     printCell cs
                                      where Just a = c  


-- B2: readSudoku file reads from the file, and either delivers it, or stops
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
                toLists strs = map toMaybe strs

                toMaybe :: String -> [Maybe Int]
                toMaybe str = map makeMaybe str
                 where
                   makeMaybe :: Char -> Maybe Int
                   makeMaybe '.'    = Nothing
                   makeMaybe c      = Just (digitToInt c)                
-------------------------------------------------------------------------

-- C1: cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency $ [(1,return $ Just a) | a <- [1..9]] ++ [(9,return Nothing)]

-- C2: an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- C3: QuickCheck properties
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

prop_Sudoku2 :: Sudoku -> Property
prop_Sudoku2 s =  collect s (isSudoku s)
-------------------------------------------------------------------------
type Block = [Maybe Int]

-- D1: Checks whether a block has a digit twice.
isOkayBlock :: Block -> Bool
isOkayBlock b = (length b == length (nubBy areEqual b)) && length b == 9
 where 
   areEqual :: Maybe Int -> Maybe Int -> Bool
   areEqual (Just a)(Just b) = (a == b)
   areEqual  _ _             = False

-- D2: Returns all blocks of a Sudoku
getRows :: Sudoku -> [Block]
getRows (Sudoku lists) = lists

getColumns :: Sudoku -> [Block]
getColumns (Sudoku ls) = transpose ls

getSquares :: Sudoku -> [Block]
getSquares (Sudoku ([]:([]:([]:[])))) = []
getSquares (Sudoku ([]:([]:([]:xs)))) = getSquares (Sudoku xs)    -- change to next row of squares
getSquares (Sudoku (l1:(l2:(l3:xs)))) = (h1++h2++h3) : (getSquares (Sudoku (t1:(t2:(t3:xs)))))
                                    where
                                        h1 = take 3 l1
                                        h2 = take 3 l2
                                        h3 = take 3 l3
                                        t1 = drop 3 l1
                                        t2 = drop 3 l2
                                        t3 = drop 3 l3

blocks :: Sudoku -> [Block]
blocks s = (getRows s) ++ (getColumns s) ++ (getSquares s)

prop_blocks :: Sudoku -> Bool
prop_blocks s = length (blocks s) == 3*9 && and [length b == 9 | b <- (blocks s)]

-- D3: D1 for all blocks
isOkay :: Sudoku -> Bool
isOkay s = and [isOkayBlock b | b <- (blocks s)]


-------------------------------------------------------------------------
type Pos = (Int,Int)
-- E1: Returns a list of blank positions.
blanks :: Sudoku -> [Pos]
blanks = undefined
