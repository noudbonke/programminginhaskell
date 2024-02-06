{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p 

item :: Parser Char
item = P (\case 
          [] -> []
          (x:xs) -> [(x,xs)])

instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of 
                          [] -> []
                          [(v, out)] -> [(g v, out)])

instance Applicative Parser where
    --pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of 
                    [] -> []
                    [(g, out)] -> parse (fmap g px) out)
{-
three :: Parser (Char, Char)
three = g <$> item <*> item <*> item
        where g x y z = (x, z)
-}

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                        [] -> []
                        [(v, out)] -> parse (f v) out)

three :: Parser (Char, Char)
three = do x <- item
           item
           z <- item
           return (x,z)

instance Alternative Parser where
  --empty :: Parser a
  empty = P (const [])

  -- (<|>) :: Parser a -> Parser a -> Parser a 
  p <|> q = P (\inp -> case parse p inp of 
                        [] -> parse q inp
                        [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)
