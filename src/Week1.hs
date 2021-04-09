{-
 - CIS 194 Homework 1(https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf)
 - Translated and solved by BlackCloud(https://blackcloud37.tech)
 - To see the lecture in Chinese, see https://blackcloud37.tech/posts/fb3ef408/
 -}

{-
  When solving the homework, strive to create not just code that
  works, but code that is stylish and concise. See the style guide on
  the website for some general guidelines. Try to write small functions
  which perform just a single task, and then combine those smaller
  pieces to create more complex functions. Don’t repeat yourself: write
  one function for each logical task, and reuse functions as necessary.
  Be sure to write functions with exactly the specified name and
  type signature for each exercise (to help us test your code). You may
  create additional helper functions with whatever names and type
  signatures you wish.                                               
  -}

module Week1
    ( week1
    ) where

import Libs

week1 :: IO ()
week1 = do
  putStrLn "Week1 Homeworks Here!"
  putStrLn "Checking Exercise 1 ..."
  check (toDigits 1234 == [1,2,3,4])
  check (toDigitsRev 1234 == [4,3,2,1])
  check (null (toDigits 0))
  check (null (toDigits (-17)))
  putStrLn "-----------------------"
  putStrLn "Checking Exercise 2 ..."
  check (doubleEveryOther [8,7,6,5] == [16,7,12,5])
  check (doubleEveryOther [1,2,3] == [1,4,3])
  putStrLn "-----------------------" 
  putStrLn "Checking Exercise 3 ..."
  check (sumDigits [16,7,12,5] == 22)
  putStrLn "-----------------------"
  putStrLn "Checking Exercise 4 ..."
  check (validate 4012888888881881)
  check (not (validate 4012888888881882))
  putStrLn "-----------------------"
  putStrLn "Checking Exercise 5"
  check (hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")])
  check (length (hanoi 15 "a" "b" "c") == 32767)


----------------------------------------------------------------
--------------- Validating Credit Card Numbers -----------------
----------------------------------------------------------------
-- Exercise 1 --------------------------------------------------
-- Example: toDigits 1234 == [1,2,3,4]
-- Example: toDigitsRev 1234 == [4,3,2,1]
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []

lastDigit :: Integer -> Integer
lastDigit n
  | n <= 0    = 0
  | otherwise = mod n 10

rightShiftDigit :: Integer -> Integer 
rightShiftDigit n
  | n <= 0    = 0
  | otherwise = div n 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = lastDigit n : toDigitsRev (rightShiftDigit n)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Exercise 2 --------------------------------------------------
-- Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- Example: doubleEveryOther [1,2,3] == [1,4,3]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []   = []
doubleEveryOther [x]  = [x]
doubleEveryOther all@(x:y:zs)
  | even (length all) = x*2 : y : doubleEveryOther zs
  | otherwise         = x   : doubleEveryOther (y:zs)  -- skip the first number

-- Exercise 3 --------------------------------------------------
-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22

flattenDigits :: [Integer] -> [Integer] 
flattenDigits []     = []
flattenDigits (x:xs) = toDigits x ++ flattenDigits xs

sumDigits :: [Integer] -> Integer 
sumDigits xs = sum (flattenDigits xs)

-- Exercise 4 --------------------------------------------------
-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False

validate :: Integer -> Bool 
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

----------------------------------------------------------------
-------------------- The Towers of Hanoi -----------------------
----------------------------------------------------------------

type Peg = String 
type Move = (Peg, Peg)

-- Exercise 5 --------------------------------------------------

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from temp to = [(from, to)] 
hanoi n a b c = hanoi (n-1) a b c ++ hanoi 1 a c b ++ hanoi (n-1) c a b

-- Exercise 6 (Optional) ---------------------------------------
-- Unfinished