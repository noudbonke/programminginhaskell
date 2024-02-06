import System.IO
--exercise 1
putStr :: String -> IO ()
putStr xs = sequence_ [putChar x | x <- xs]

--exercise 4
adder :: IO ()
{-
adder = do Main.putStr "How many Numbers? "
           n <- getLine 
           xs <- getNums (read n :: Int)
           Main.putStr "The total is "
           putStrLn (show (sum xs))


getNums :: Int -> IO [Int]
getNums 0 = return []
getNums n = do x <- getLine
               xs <- getNums (n-1)
               return ((read x :: Int) : xs)
-}
--exercise 5
adder = do Main.putStr "How many Numbers? "
           n <- getLine 
           xs <- sequence (replicate (read n :: Int) getLine)
           Main.putStr "The total is "
           putStrLn (show (sum (map (\x -> read x :: Int) xs)))

--exercise 6 
readLine :: IO String 
readLine = readLineInput ""

readLineInput :: String -> IO String
readLineInput xs = do 
                hSetEcho stdin False
                x <- getChar
                case x of
                  '\DEL' -> 
                    if null xs then readLineInput ""
                    else do
                      Main.putStr "\b \b"
                      readLineInput (init xs)
                  '\n' -> do 
                    putChar '\n'
                    return xs
                  _ -> do 
                    putChar x
                    readLineInput (xs ++ [x])
