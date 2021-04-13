{-
 - CIS 194 Homework 4(https://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf)
 - Translated and solved by BlackCloud(https://blackcloud37.tech)
 - To see the lecture in Chinese, see https://blackcloud37.tech/posts/a3f48c69/
 -}

module Week4 where

----------------------------------------------------------------
------------------ Wholemeal programming -----------------------
----------------------------------------------------------------
-- Exercise 1 --------------------------------------------------

fun1 :: [Integer] -> Integer 
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer 
fun1' = foldr (\x acc -> (x-2)*acc) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


fun2' :: Integer -> Integer 
fun2' = sum . filter even
            . takeWhile (>1)
            . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

----------------------------------------------------------------
-------------------- Folding with trees ------------------------
----------------------------------------------------------------
-- Exercise 2 --------------------------------------------------

-- Reference: https://github.com/glennrfisher/cis194-haskell/blob/master/04%20Higher%20Order%20Programming/homework4.hs
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h Leaf y Leaf) = Node 1 (insert x Leaf) y Leaf
insert x (Node h Leaf y right) = Node h (insert x Leaf) y right
insert x (Node h left y Leaf) = Node h left y (insert x Leaf)
insert x (Node h left y right) =
    let (leftH, rightH) = (height left, height right) in
    case compare leftH rightH of
        LT -> Node h (insert x left) y right
        GT -> Node h left y (insert x right)
        EQ -> Node (1 + height right') left y right'
                where right' = insert x right

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

----------------------------------------------------------------
------------------------ More folds ----------------------------
----------------------------------------------------------------
-- Exercise 3 --------------------------------------------------

-- 3.1
xor :: [Bool] -> Bool 
xor = odd . length . filter (==True)

-- 3.2
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- 3.3
myFoldl :: (a -> b -> a) -> a -> [b] -> a 
myFoldl f init xs = foldr (flip f) init (reverse xs)

----------------------------------------------------------------
---------------------- Finding primes --------------------------
----------------------------------------------------------------
-- Exercise 4 --------------------------------------------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
    let exclude = [i+j+2*i*j | i <- [1..n], j <- [1..i], i+j+2*i*j <= n] in
      [2*i+1 | i <- [1..n], i `notElem` exclude] 