module Lab1 where
import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 1 ---------------------------------
-- "power n k" takes k+1 steps


-- Part 2 --------------------------------
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product [n | x <- [1..k]]


-- Part 3 --------------------------------
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | even k    = power2 (n * n) (div k 2)
           | otherwise = n * power2 n (k - 1) 


-- Part 4 --------------------------------
-- A --
--[(n,k)]
inputs = [(1, 1),
          (1, 0),
          (0, 1),
          (99999, 99999),
          (2, (-1)),
          ((-2), 1),
          ((-2), 0),
--          (1.1, 1.2),  -- commented so the module can load
--          (1.1, 1),    -- without Frac Integer error
--          (1, 1.1),
          (0 ,0 )]

-- B --
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k) == (power1 n k) && 
                  (power n k) == (power2 n k)


-- C --
test_all = and [ prop_powers (fst x) (snd x) | x <- inputs ]

-- D --
-- n^(-k) == 1/(n^k)
prop_powers' n k = let k'= abs k in
                    (power n k') == (power1 n k') &&
                    (power n k') == (power2 n k')
