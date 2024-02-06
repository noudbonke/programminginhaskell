import Data.List

main :: IO ()
--modified for exercise 4
main = do 
  print (solutions'' [1, 3, 7, 10, 25, 50] 765)
  --print (length [e | cs <- choices [1, 3, 7, 10, 25, 50], e <- exprs cs]) 
  --print (length [r | cs <- choices [1, 3, 7, 10, 25, 50], e <- exprs cs, r <- eval e]) 
  print (nearestSolutions [1, 3, 7, 10, 25, 50] 831)

--modified for exercise 6a 
data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  --added for exercise 6a
  show Exp = "^"
{-
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
-}

valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y 
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0
--added for exercise 6a
valid Exp x y = x >= 1 && y >= 1


--added for exercise 5
{-
valid Add x y = True 
valid Sub x y = True 
valid Mul x y = True 
valid Div x y = y /= 0 && x `mod` y == 0
-}

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
--added for exercise 6a
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where 
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                      brak (Val n) = show n 
                      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int] 
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs)) 

--modified for exercise 1
choices :: [a] -> [[a]]
{-
choices = concat . map perms . subs
-}
choices xs = [zs | ys <- subs xs, zs <- perms ys]

--added for exercise 2
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys) 
  | x == y = ys
  | otherwise = y : removeFirst x ys

--added for exercise 2
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys = x `elem` ys && isChoice xs (removeFirst x ys)

solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = 
  [e | ns' <- choices ns, (e, m) <- results ns', m == n]


--added for exercise 6b
--modified for exercise 6c
nearestSolutions :: [Int] -> Int -> [Expr]
nearestSolutions ns n =
  sortOn (length . values)  (map snd (takeWhile (\x -> fst x == closest) allSolutions))
  where allSolutions = sortOn fst [(abs(m-n), e) | ns' <- choices ns, (e, m) <- results ns'] 
        closest = (fst . head) allSolutions
