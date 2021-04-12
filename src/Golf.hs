{-
 - CIS 194 Homework 3(https://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf)
 - Translated and solved by BlackCloud(https://blackcloud37.tech)
 - To see the lecture in Chinese, see https://blackcloud37.tech/posts/5f294974/
 -}

module Golf where

import Data.List (maximum)
import Libs

----------------------------------------------------------------
------------------------ Code golf -----------------------------
----------------------------------------------------------------
-- Exercise 1 --------------------------------------------------

-- Reference: https://stackoverflow.com/questions/2026912/how-to-get-every-nth-element-of-an-infinite-list-in-haskell
--   see also https://codereview.stackexchange.com/questions/85095/upenn-homework-3-skips-function

every xs n  = case drop (n-1) xs of
              []     -> []
              y : ys -> y : every ys n

skips :: [a] -> [[a]]
skips xs = map (every xs) [1.. length xs]

-- Exercise 2 --------------------------------------------------

isMaxima :: [Integer] -> (Integer, Int) -> Bool 
isMaxima xs (_,idx) = idx /= 0 && 
                      idx /= length xs - 1 &&
                      xs !! idx >= xs !! idx - 1 &&
                      xs !! idx >= xs !! idx + 1

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map fst (filter (isMaxima xs) (zip xs [0..]))


-- Exercise 3 --------------------------------------------------

count xs x = length $ filter (x==) xs

counts xs = map (count xs) [0..9]

genLine ns n = map (\x -> if x >= n then '*' else ' ') ns

histogram :: [Integer] -> String
histogram xs = unlines (reverse (map (genLine cnts) [1 .. (maximum cnts)])) 
  ++ "==========\n0123456789\n" 
  where cnts = counts xs