import Data.Foldable
--Exercise 1
{-
instance (Monoid a, Monoid b) => Monoid (a,b) where
   --mempty :: (a, b)
   mempty = (mempty, mempty) 

   --mappend :: (a,b) -> (a,b) -> (a,b)
   (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)
-}

{-
--Exercise 2
instance Monoid b => Monoid (a -> b) where
   --mempty :: (a -> b)
   mempty = const mempty 

   --mappend :: (a -> b) -> (a -> b) -> (a -> b)
   f `mappend` g = \x -> f x `mappend` g x
-}

{-
--Exercise 3
instance Foldable Maybe where
   --fold :: Monoid a => Maybe a -> a
   fold (Just x) = x
   fold Nothing = mempty

   --foldMap :: Monoid b => (a -> b) -> Maybe a -> b
   foldMap f (Just x) = f x
   foldMap _ Nothing = mempty

   --foldr :: (a -> b -> b) -> b -> Maybe a -> b
   foldr f y (Just x) = f x y
   foldr _ y Nothing = y

   --foldl :: (a -> b -> a) -> a -> Maybe b -> a
   foldl f x (Just y) = f x y
   foldl _ x Nothing = x
-}
{-
instance Traversable Maybe where
   --traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
   traverse g (Just x) = fmap Just (g x)
   traverse _ Nothing = pure Nothing
-}

--Exercise 4
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Foldable Tree where
   --fold :: Monoid a => Tree a -> a
   fold Leaf = mempty 
   fold (Node l x r) = fold l `mappend` x `mappend` fold r

   --foldMap :: Monoid b => (a -> b) -> Tree a -> b
   foldMap _ Leaf = mempty 
   foldMap g (Node l x r) = foldMap g l `mappend` g x `mappend` foldMap g r

   --foldr :: (a -> b -> b) -> b -> Tree a -> b
   foldr _ y Leaf = y 
   foldr f y (Node l x r) = foldr f (f x (foldr f y r)) l  

   --foldl :: (a -> b -> a) -> a -> Tree b -> a
   foldl _ x Leaf = x 
   foldl f x (Node l y r) = foldl f (f (foldl f x l) y) r  

instance Functor Tree where
   --fmap :: (a -> b) -> Tree a -> Tree b
   fmap _ Leaf = Leaf
   fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Traversable Tree where
   --traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
   traverse _ Leaf = pure Leaf
   traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r

--Exercise 5
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> [x | p x])

