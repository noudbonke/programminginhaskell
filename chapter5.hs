factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']
--1
hundredsquares :: [Int]
hundredsquares = [x^2 | x <- [1..100]]

--2 
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

--3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

--4
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

--5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--6
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == 2*x] --2*x as number itself is in factors

--7
sometuples :: [(Int, Int)]
sometuples = concat [[(x, y) | y <- [3,4]] | x <- [1, 2]]

--8
positions :: Eq a => a -> [a] -> [Int] 
positions x xs = find x (zip xs [0..])

--9
scalarproduct :: [Int] -> [Int] -> Int 
scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]
