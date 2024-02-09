--defined
data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr
            deriving Show

eval :: Expr -> Maybe Int
eval (Val n) = Just n
--reformatted in monadic notation 
eval (Add x y) = do 
                    n <- eval x
                    m <- eval y
                    return (n+m)
eval Throw = Nothing
eval (Catch x h) = case eval x of 
                    Just n -> Just n
                    Nothing -> eval h

type Stack = [Maybe Int]

--calculated
data Code = PUSH Int Code | THROW Code | ADD Code | CATCH Code | HALT
            deriving Show

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' Throw c = THROW c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Catch x h) c = comp' x (comp' h (CATCH c))


comp :: Expr -> Code
comp e = comp' e HALT

exec :: Code -> Stack -> Stack
exec (PUSH n c) s = exec c (Just n : s)
exec (THROW c) s = exec c (Nothing : s)
exec (ADD c) (y : x : s) = exec c ((do 
                                    n <- x 
                                    m <- y 
                                    return (n + m)) : s)
exec (CATCH c) (h : x : s) = exec c ((case x of
                                        Just n -> Just n
                                        Nothing -> h) : s)
exec HALT s = s
