module Sudoku where

import Test.QuickCheck hiding ((.&.))
import System.IO
import Data.Char
import Data.List
import Data.Maybe
import Data.Bits ((.&.), complement)

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

getSquares :: Sudoku -> [Block]  -- squares are retrieved left->right, top->bottom
getSquares (Sudoku [])              = []
getSquares (Sudoku ([]:(_ :(_ :xs)))) = getSquares (Sudoku xs)    -- change to next row of squares
getSquares (Sudoku (r1:(r2:(r3:xs)))) = square : getSquares (Sudoku (leftovers ++ xs) ) -- change to next column of squares
                                    where
                                        square    = concat $ map (take 3) [r1,r2,r3]
                                        leftovers =          map (drop 3) [r1,r2,r3]

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
blanks s = concat [[(r,c) |c <- [0..8], ((rows s !! r) !! c)==Nothing] | r <- [0..8]]

prop_checkBlanks :: Sudoku -> Bool
prop_checkBlanks s = and [rows s !! r !! c == Nothing | (r,c) <- (blanks s)]

--E2: Replaces a value in a list.
(!!=) :: [a] -> (Int,a) -> [a]
[]      !!= _      = []
(x:xs)  !!= (0, v) = v: xs
(x:xs)  !!= (n, v) = x: xs !!= (n-1, v)

prop_size :: [a] -> (Int,a)-> Bool
prop_size list (ind, val)= length list == length (list !!= (ind,val)) 


--E3: Updates a Sudoku cell
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku lists) (r,c) maybe = Sudoku $lists !!= (r, ((lists !! r) !!= (c,maybe)))

prop_checkVal :: Sudoku -> Pos -> Maybe Int -> Bool
prop_checkVal s (r,c) v = v == rows (update s (r',c') v) !! r' !! c'
                      where r' = r `mod` 9
                            c' = c `mod` 9


-- E4: Returns candidate values of a cell
candidates :: Sudoku -> Pos -> [Int]
--candidates s (r,c) = (([1..9] \\ row) \\ col) \\ squ    -- quadratic version
candidates s (r,c) = bits (foldl f (foldl f (foldl f 511 squ) col) row)    -- linear version encode list to a number
      where row = catMaybes $ getRows    s !! r
            col = catMaybes $ getColumns s !! c
            -- assuming squares are retrieved left->right, top->bottom
            squ = catMaybes $ getSquares s !! ( (r `div` 3) * 3 + (c `div` 3))
            f   = \a b -> a .&. (complement $ 2^(b-1))

            bits :: Int -> [Int]
            bits n = bits' n 1
              where bits' :: Int -> Int -> [Int]
                    bits' 0 _             = []
                    bits' n b | even n    =     bits' (n `div` 2) (b+1)
                              | otherwise = b : bits' (n `div` 2) (b+1)

prop_relateFuncs :: Sudoku -> Pos -> Property
prop_relateFuncs sud (r,c) = (isSudoku sud && isOkay sud) ==> 
                        and[isSudoku upd && isOkay upd| upd <- upSudList]
  where
    r'        = r `mod` 9
    c'        = c `mod` 9
    candList  = candidates sud (r',c')
    upSudList = map (update sud (r',c')) ([Just a | a <- candList])

------------------------------------------------------------------------
-- F1: Implementation of the function that solves sudokus
solve :: Sudoku -> Maybe Sudoku
solve s | not (isOkay s) || not (isSudoku s) = Nothing
        | otherwise                          = solve' s


solve' :: Sudoku -> Maybe Sudoku
solve' s | null blanks_     = Just s
         | otherwise        = head ( filter (/=Nothing) (map (solve' . (update s pos)) candidates_)
                                     ++ [Nothing] )   -- no solution found
            where blanks_     = blanks s
                  pos         = blanks_ !! 0
                  candidates_ = map Just (candidates s pos) 

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
isSolutionOf a b =  (isSudoku a) && (isOkay a) &&
                    isSolved a && [map equalEl (zip (fst p) (snd p)) | p <- zip (rows a) (rows b)] == rows b
                where equalEl :: Eq a => (Maybe a, Maybe a) -> Maybe a
                      equalEl (p,q) | p == q = p
                      equalEl _              = Nothing

-- F4: property that says that the function solve is sound
prop_SolveSound :: Sudoku -> Property    -- shows number of default numbers in sudoku
prop_SolveSound s | s' == Nothing = collect s' True
                  | otherwise     = collect s' (isSolutionOf (fromJust s') s)
              where s' = solve s
