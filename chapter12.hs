import GHC.RTS.Flags (DebugFlags(stm), GCFlags (statsFile))
import Data.ByteString (stripPrefix)
--Exercise 1
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
    --fmap :: (a -> b) -> Tree a -> Tree b
    fmap g Leaf = Leaf
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

--Exercise 2
{-
instance Functor ((->) a) where
    --fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap = (.) 
-}

--Exercise 3
{-
instance Applicative ((->) a) where
    --pure :: b -> (a -> b) 
    pure = const 
    --(<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
    g <*> h = \x -> g x (h x)
-}

--Exercise 4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    --fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
    --pure :: a -> ZipList a
    pure x = Z (repeat x)

    --(<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs] 

--Exercise 5
{-
1. x :: f a 
2. x :: f a, g :: f (a -> b)
3. x :: f (a -> b), y :: a, g :: (a -> b)
4. x :: f (b -> c), y :: f (a -> b), z :: a
 -}

 --Exercise 6
{-
instance Monad ((->) a) where
    --(>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c) 
    (f >>= g) x = g (f x) x  
-}

--Exercise 7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    --fmap :: (a -> b) -> Expr a -> Expr b
    fmap g (Var x) = Var (g x)
    fmap _ (Val n) = Val n 
    fmap g (Add e e') = Add (fmap g e) (fmap g e') 

instance Applicative Expr where
    --pure :: a -> Expr a
    pure = Var 
    --(<*>) :: Expr (a -> b) -> Expr a -> Expr b
    _ <*> Val n = Val n
    Val n <*> _ = Val n
    Var g <*> Var x = Var (g x) 
    Var g <*> Add e e' = Add (fmap g e) (fmap g e')  
    Add eg eh <*> e = Add (eg <*> e) (eh <*> e)

instance Monad Expr where
    --(>>=) :: Expr a -> (a -> Expr b) -> Expr b
    Val n >>= _ = Val n
    Var x >>= g = g x
    Add e e' >>= g = Add (e >>= g) (e' >>= g)

{- The bind operator for Expr applies a function to each variable in the expression. 
 - An example is given below. -}

--example = Add (Var 9) (Var 16)
example = Add (Var 3) (Var 4) >>= \x -> Var (x ^ 2) 

--Exercise 8

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
    --fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do x <- st 
                   return (g x)

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x, s))
    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do f <- stf 
                     x <- stx 
                     return (f x)

instance Monad ST where
    --(>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s ->
        let (x, s') = app st s in app (f x) s')

