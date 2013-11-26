module Sudoku where

import Test.QuickCheck
import System.IO
import Data.Char
import Data.List
import Data.Maybe

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
{-isSolved :: Sudoku -> Bool
isSolved (Sudoku list) = and [hasNoth a | a <- list]
 where 
   hasNoth :: [Maybe Int] -> Bool
   hasNoth list = and [False | a <- list , (a == Nothing)]-}
isSolved = all (all (/= Nothing)) . rows
-------------------------------------------------------------------------

-- B1: printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = mapM_ (\r -> do mapM_ printCell r; putStr "\n") (rows s)
  where 
    -- prints out each cell of the Sudoku
    printCell :: Maybe Int -> IO ()
    printCell Nothing  = do putStr "."
    printCell (Just a) = do putStr $ show a


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
             return $Sudoku $toLists $lines content
              where
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
getSquares (Sudoku (l1:(l2:(l3:xs)))) = (h1++h2++h3) : (getSquares (Sudoku (t1:(t2:(t3:xs))))) -- change to next column of squares
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
blanks sud = getPoses 0 (getRows sud)
 where
  getPoses :: Int -> [[Maybe Int]] -> [Pos]
  getPoses _ []     = []
  getPoses r (l:ls) = getPos r 0 l ++ getPoses (r+1) ls

  getPos :: Int -> Int -> [Maybe Int] -> [Pos]
  getPos _ _ []           = []
  getPos m n (Nothing:cs) = [(m,n)]++ getPos m (n+1) cs
  getPos m n (_:cs)       = getPos m (n+1) cs

-- non-exhaustive patterns!!!
checkCells :: [Pos] -> [[Maybe Int]] -> [Bool]
checkcells  [] _         = []
checkCells ((r,c):ps) ls = [((ls !! r) !! c)== Nothing] ++ checkCells ps ls

prop_checkBlanks :: Bool
prop_checkBlanks = and (checkCells (blanks allBlankSudoku) (rows allBlankSudoku))

--E2: Replaces a value in a list.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] (_,_)                                        = []
(!!=) list (ind,_) | (ind >= length list) || ind < 0  = list
(!!=) list (ind,val)                                  = l1 ++ [val] ++ es
  where
   (l1, (e:es))= splitAt ind list 

prop_size :: [a] -> (Int,a)-> Bool
prop_size list (ind, val)= length list == length ((!!=) list (ind,val)) 

--E3: Updates a Sudoku cell
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku lists) (r,c) maybe = Sudoku $(!!=)lists (r,((!!=)(lists !! r) (c,maybe)))

prop_checkVal :: Sudoku -> Pos -> Maybe Int -> Bool
prop_checkVal sud (r,c) Nothing  |(r `elem` [0..8]) && (c `elem` [0..8])                      = ((rows (update sud (r,c) Nothing)) !! r) !! c == Nothing
                                 |otherwise                                                   = prop_checkVal sud (1,1) Nothing
prop_checkVal sud (r,c) (Just a) |a `elem` [1..9] && ((r `elem` [0..8]) && (c `elem` [0..8])) = ((rows (update sud (r,c) (Just a))) !! r) !! c == (Just a)
                                 |otherwise                                                   = prop_checkVal sud (1,1) (Just 2)


candidates :: Sudoku -> Pos -> [Int]
candidates = undefined 


------------------------------------------------------------------------
-- F1: Implementation of the function that solves sudokus
solve :: Sudoku -> Maybe Sudoku
solve s | not (isOkay s) || not (isSudoku s) = Nothing
        | otherwise                          = solve' s


solve' :: Sudoku -> Maybe Sudoku
solve' s | null blanks_     = Just s
         | null candidates_ = Nothing
         | otherwise        = solve'' s pos candidates_

            where blanks_     = blanks s
                  pos         = blanks_ !! 0
                  candidates_ = map Just (candidates s pos)
                  
                  -- tries sudoku solutions with given candidates to position Pos
                  solve'' :: Sudoku -> Pos -> [Maybe Int] -> Maybe Sudoku
                  solve'' s p [] = Nothing
                  solve'' s p (c:cs) | solution == Nothing  = solve'' s p cs
                                     | otherwise            = solution 
                                        where solution = solve' (update s p c) 


-- F2: solving sudoku from file
readAndSolve :: FilePath -> IO ()
readAndSolve path = do 
                    sud <- readSudoku path
                    let s = solve sud
                    if s == Nothing
                    then do
                      putStrLn "(no solution)"
                    else do
                      printSudoku (fromJust s)

-- F3: checks if first sudoku is a valid solution of the second one
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf a b = (isSudoku a) && (isOkay a) && and [(commonElements (fst p) (snd p)) == (snd p)
                                                        | p <- (zip (rows a) (rows b)) ]
                                                        
-- returns list with same length as input lists and the common elements at
-- same position
commonElements :: Eq a => [Maybe a] -> [Maybe a] -> [Maybe a]
commonElements []     []     = []
commonElements (a:as) (b:bs) | a == b    = a       : (commonElements as bs)
                             | otherwise = Nothing : (commonElements as bs)


-- F4: property that says that the function solve is sound
prop_SolveSound :: Sudoku -> Property    -- shows number of default numbers in sudoku
prop_SolveSound s = collect ((length . (filter (/= Nothing))) (concat $ rows s)) (isSolutionOf (fromJust (solve s)) (s))