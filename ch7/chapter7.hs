--1
{- 
map f (filter p xs)
-}

--2
--a
all :: (a -> Bool) -> [a] -> Bool
all p = and . Prelude.map p

--b
any :: (a -> Bool) -> [a] -> Bool
any p = or . Prelude.map p 

--c
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x = x : Main.takeWhile p xs
                   | otherwise = []

--d
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) | p x = Main.dropWhile p xs
                   | otherwise = x:xs 

--3
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x:xs else xs) []

--4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

--5
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x, y)

uncurry ::  (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y 

--6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int
chop8 :: [Bit] -> [[Bit]]               
chop8 = unfold null (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail 

iterate :: (a -> a) -> a -> [a]
iterate = unfold (const False) id  

--9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = [] 
altMap f0 f1 (x:xs) = f0 x : altMap f1 f0 xs

--10
luhndouble :: Int -> Int
luhndouble x = if x >= 5 then 2*x - 9 else 2*x

luhn :: [Int] -> Bool 
luhn xs = sum (altMap luhndouble id xs) `mod` 10 == 0 
