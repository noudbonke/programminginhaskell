--Exercise 4
fibs :: [Integer]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

--Exercise 5
data Tree a = Leaf | Node (Tree a) a (Tree a) 
              deriving Show

repeat :: a -> Tree a
repeat x = t where t = Node t x t 

take :: Int -> Tree a -> Tree a
take 0 _ = Leaf
take _ Leaf = Leaf
take n (Node l x r) = Node (Main.take (n-1) l) x (Main.take (n-1) r)

replicate :: Int -> a -> Tree a
replicate n = Main.take n . Main.repeat

--Exercise 6
next :: Double -> Double -> Double
next n a = (a + n/a) / 2 

tol :: (Double, Double) -> Bool
tol (x, y) = abs (x - y) >= 0.00001

sqroot :: Double -> Double
sqroot n = fst (head (dropWhile tol (zip ns (tail ns))))
           where ns = iterate (next n) 1.0



