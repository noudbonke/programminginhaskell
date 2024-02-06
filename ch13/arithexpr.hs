module ArithExpr where
import Control.Applicative
import Parser

--modified for exercise 6
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr 
             return (t + e)
           <|> do symbol "-"
                  e <- expr 
                  return (t - e)
                <|> return t
          
--modified for exercise 6-7
term :: Parser Int
term = do x <- expo
          do symbol "*"
             t <- term 
             return (x * t)
           <|> do symbol "/"
                  t <- term 
                  return (x `div` t)
                <|> return x

--added for exercise 7
expo :: Parser Int
expo = do f <- factor
          do symbol "^"
             x <- expo
             return (f ^ x)
           <|> return f

--modified for exercise 6-7
factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> int

--modified for exercise 6
eval :: String -> Int
eval xs = case parse expr xs of 
            [(e, [])] -> e
            [(_, out)] -> error ("Unused input " ++ out)
            [] -> error "Invalid input"

--added for exercise 5
--data Expr = Val Int | Add Expr Expr | Mul Expr Expr

--modified for exercise 5
{-
expr :: Parser Expr
expr = do t <- term
          do symbol "+"
             e <- expr 
             return (Add t e)
           <|> return t
          
term :: Parser Expr
term = do f <- factor
          do symbol "*"
             t <- term 
             return (Mul f t)
           <|> return f

factor :: Parser Expr
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> fmap Val natural

eval :: String -> Expr
eval xs = case parse expr xs of 
            [(e, [])] -> e
            [(_, out)] -> error ("Unused input " ++ out)
            [] -> error "Invalid input"
-}
