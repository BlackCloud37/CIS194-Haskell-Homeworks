{-
 - CIS 194 Homework 2(https://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf)
 - for attached files, see https://www.seas.upenn.edu/~cis194/spring13/lectures.html
 - Translated and solved by BlackCloud(https://blackcloud37.tech)
 - To see the lecture in Chinese, see https://blackcloud37.tech/posts/786f7a8f/
 -}
module Week2.Week2 ( week2 ) where

import Libs
import Week2.Log
import Data.Char (isDigit)


-- Note: Only Exercise1-5 was finished, 
--       and I only test my program on sample.log with whatWentWrong (at least, it ouputs correctly)
week2 :: IO () 
week2 = do 
  putStrLn "Week2 Homeworks Here!"
  whatWentWrong <- testWhatWentWrong parse whatWentWrong "./src/Week2/sample.log"
  print whatWentWrong -- outputs: ["Way too many pickles","Bad pickle-flange interaction detected","Flange failed!"]


----------------------------------------------------------------
---------------------- Log file parsing ------------------------
----------------------------------------------------------------
{-
Don’t reinvent the wheel! (That’s so last week.) 
Use Prelude functions to make your solution as concise, high-level, and functional as
possible. For example, to convert a String like "562" into an Int, you
can use the read function. Other functions which may (or may not)
be useful to you include lines, words, unwords, take, drop, and (.).
-}

-- Exercise 1 --------------------------------------------------
-- Note: this parseMessage function didn't handle the case that
--     level of error or timestamp were non-intergal strings

parseMessage :: String -> LogMessage 
parseMessage line =
  case words line of
    ("E":l:ts:t) -> LogMessage (Error (read l :: Int)) (read ts :: Int) (unwords t)
    ("I":ts:t)   -> LogMessage Info (read ts :: Int) (unwords t)
    ("W":ts:t)   -> LogMessage Warning (read ts :: Int) (unwords t)
    t            -> Unknown (unwords t)


parse :: String -> [LogMessage]
parse [] = []
parse s  = parseMessage l : parse (unlines ls)
  where (l:ls) = lines s
  
-- Exercise 2 --------------------------------------------------

insert :: LogMessage -> MessageTree -> MessageTree 
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf 
insert msg@(LogMessage _ ts _) (Node l nmsg@(LogMessage _ nts _) r)
  | ts < nts = Node (insert msg l) nmsg r
  | ts >= nts = Node l nmsg (insert msg r)

-- Exercise 3 --------------------------------------------------

build :: [LogMessage] -> MessageTree 
build [] = Leaf 
build (x:xs) = insert x (build xs)

-- Exercise 4 --------------------------------------------------

inOrder :: MessageTree -> [LogMessage ]
inOrder Leaf = []
inOrder (Node l n r) = inOrder l ++ [n] ++ inOrder r

-- Exercise 5 --------------------------------------------------

collectErrors :: [LogMessage ] -> [String ]
collectErrors [] = []
collectErrors ((LogMessage (Error level) _ t ):ms)
  | level >= 50 = t : collectErrors ms
  | otherwise   = collectErrors ms
collectErrors (_:ms) = collectErrors ms -- non-error messages

whatWentWrong :: [LogMessage ] -> [String ]
whatWentWrong msgs = collectErrors (inOrder (build msgs))