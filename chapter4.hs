--1
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2 

--2
third :: [a] -> a
--third xs = head (tail (tail xs))
--third xs = xs !! 2
third (_:_:x:_) = x

--3
safetail :: [a] -> [a]
--safetail xs = if null xs then [] else tail xs

--safetail xs | null xs = []
--            | otherwise = tail xs
--
safetail [] = []
safetail (_:xs) = xs 

--4
(||) :: Bool -> Bool -> Bool
{-
option 1: truth table

False || False = False
_ || _ = True

True || _ = True
False || b = b
-}
b || c | b == c = b 
       | otherwise = True

--5 
(&&) :: Bool -> Bool -> Bool
--a && b = if a then if b then True else False else False

--6
a && b = if a then b else False

--7 
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))

--8
luhndouble :: Int -> Int
luhndouble x = if x >= 5 then 2*x - 9 else 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = ((luhndouble a + b + luhndouble c + d) `mod` 10) == 0 
