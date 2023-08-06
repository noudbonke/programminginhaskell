--1
data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add (mult m n) n

--2
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y 
occurs x (Node l y r) = case compare x y of 
                          LT -> occurs x l
                          EQ -> True
                          GT -> occurs x r

--3                        
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

nLeaves :: Tree' a -> Int
nLeaves (Leaf' _) = 1
nLeaves (Node' l r) = nLeaves l + nLeaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (nLeaves l - nLeaves r) <= 1 && balanced l && balanced r

--4
halve :: [a] -> ([a], [a]) 
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance zs = Node' (balance xs) (balance ys) where (xs, ys) = halve zs

--5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n) = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

--6
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

--7
{-
instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Just a == Just b = a == b
  _ == _ = False

instance Eq a => Eq [a] where
  [] == [] = True
  (x:xs) == (y:ys) = (x == y) && (xs == ys)
  _ == _ = False
-}
