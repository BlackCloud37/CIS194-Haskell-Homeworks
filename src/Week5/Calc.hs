{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-
 - CIS 194 Homework 5(https://www.seas.upenn.edu/~cis194/spring13/hw/05-type-classes.pdf)
 - Translated and solved by BlackCloud(https://blackcloud37.tech)
 - To see the lecture in Chinese, see https://blackcloud37.tech/posts/db537e15/
 -}

module Week5.Calc where

import Week5.ExprT
import Week5.Parser
import Week5.StackVM
import Libs

week5 :: IO ()
week5 = do
  putStrLn "Week5 Homeworks Here!"
  putStrLn "Checking Exercise 1 ..."
  check (eval  (Week5.ExprT.Mul (Week5.ExprT.Add (Lit 2) (Lit 3)) (Lit 4)) == 20)
  putStrLn "Checking Exercise 2 ..."
  check (evalStr "2+3*4" == Just 14)
  check (evalStr "2+3*" == Nothing)
  print testInteger
  print testBool
  print testMM
  print testSat
  
----------------------------------------------------------------
----------------------- Expressions ----------------------------
----------------------------------------------------------------
-- Exercise 1 --------------------------------------------------
eval :: ExprT -> Integer 
eval (Lit i)         = i
eval (Week5.ExprT.Add exp1 exp2) = eval exp1 + eval exp2
eval (Week5.ExprT.Mul exp1 exp2) = eval exp1 * eval exp2


-- Exercise 2 --------------------------------------------------
evalStr :: String -> Maybe Integer 
evalStr s = case parseExp Lit Week5.ExprT.Add Week5.ExprT.Mul s of
  (Just e) -> Just (eval e)
  Nothing  -> Nothing

-- Exercise 3 --------------------------------------------------
class Expr a where
  lit :: Integer -> a 
  add :: a -> a -> a 
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Week5.ExprT.Lit
  add = Week5.ExprT.Add
  mul = Week5.ExprT.Mul

reify :: ExprT -> ExprT 
reify = id

-- Exercise 4 --------------------------------------------------
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n
    | n <= 0 = False 
    | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax n1) (MinMax n2) = MinMax (max n1 n2)
  mul (MinMax n1) (MinMax n2) = MinMax (min n1 n2)

newtype Mod7   = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x = Mod7 $ mod x 7
  add (Mod7 n1) (Mod7 n2) = lit (n1 + n2)
  mul (Mod7 n1) (Mod7 n2) = lit (n1 * n2) 

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 5 --------------------------------------------------
instance Expr Week5.StackVM.Program where
  lit i = [Week5.StackVM.PushI  i]
  add n1 n2 = n1 ++ n2 ++ [Week5.StackVM.Add]
  mul n1 n2 = n1 ++ n2 ++ [Week5.StackVM.Mul]

testProg = testExp :: Maybe Week5.StackVM.Program

compile :: String -> Maybe Program 
compile = parseExp lit add mul

-- Exercise 6 --------------------------------------------------
-- lazy to do