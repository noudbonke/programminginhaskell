--1
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

--2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

--3
(^) :: Int -> Int -> Int 
m ^ 0 = 1 
m ^ n = m * (m Main.^ (n-1))

--4
euclid :: Int -> Int -> Int
euclid m n | (m == n) = m
           | (m > n) = euclid n (m-n)
           | otherwise = euclid m (n-m)
           
--6
--a
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && (Main.and xs)

--b
concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ Main.concat xs 

--c
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = a : Main.replicate (n-1) a

--d
(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs Main.!! (n-1)

--e
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) = (a == x) || Main.elem a xs

--7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | (x < y) = x:(merge xs (y:ys)) 
                    | otherwise = y:(merge (x:xs) ys)
                    
--8
halve :: [a] -> ([a], [a])
halve xs | (n `mod` 2 == 0) = (Prelude.take (n `div` 2) xs, drop (n `div` 2) xs)
         | otherwise = (Prelude.take ((n-1) `div` 2) xs, drop ((n-1) `div` 2) xs)
         where 
          n = length xs
          
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    (left, right) = halve xs

--9
--a
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + Main.sum xs

take :: Int -> [a] -> [a]
take 0 _ = []
take n (x:xs) = [x] ++ Main.take (n-1) xs 

last :: [a] -> a
last [x] = x
last (_:xs) = Main.last xs

