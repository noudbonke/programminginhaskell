import Data.Char

nim :: IO ()
nim = play initial 1

next :: Int -> Int
next 1 = 2 
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
  where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
{-
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e
-}

--added for exercise 2
{-
putBoard = putBoard' 1

putBoard' :: Int -> Board -> IO ()
putBoard' r [] = return ()
putBoard' r (x:xs) = do putRow r x 
                        putBoard' (r+1) xs
-}

--added for exercise 2
putBoard xs = sequence_ [putRow n x | (n,x) <- zip [1..] xs]


getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player = 
  do newline
     putBoard board
     if finished board then 
        do newline
           putStr "Player "
           putStr (show (next player))
           putStrLn " wins!!"
     else
        do newline
           putStr "Player "
           putStrLn (show player)
           row <- getDigit "Enter a row number: "
           num <- getDigit "Stars to remove : "
           if valid board row num then 
              play (move board row num) (next player)
           else
              do newline
                 putStrLn "ERROR: Invalid move"
                 play board player
                 

