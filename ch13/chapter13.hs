import Control.Applicative
import Parser

--Exercise 1
comment :: Parser ()
comment = do string "--"
             xs <- many (sat (/= '\n'))
             return ()

--Exercise 8
--a
-- expr ::= nat | expr - nat

--b
expr' :: Parser Int
expr' = natural
         <|> do e <- expr'
                symbol "-"
                n <- natural
                return (e - n)
--c
-- The parser terminates at the first natural, not using the rest of the input.
-- When implementing the alternative the other way around, the parser gets into an infinite loop 
-- because it does not know where the expression ends.

--d
expr :: Parser Int
expr = do n <- natural
          ns <- many (do symbol "-"
                         natural)
          return (foldl (-) n ns) 

eval :: String -> Int
eval xs = case parse expr xs of 
            [(e, [])] -> e
            [(_, out)] -> error ("Unused input " ++ out)
            [] -> error "Invalid input"


