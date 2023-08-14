import Data.Char
import Data.List
import System.IO
import System.Random hiding (next)

size :: Int
size = 3

--added for exercise 4b
winLine :: Int 
winLine = 3

depth :: Int
depth = 9

--modified for exercise 4a
--modified for exercise 4c
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          c <- playerChoice "Do you want to play First (F) or second (S)? "  
          if c == "F" then play empty O (gametree empty O)
          else play empty X (gametree empty X)

--added for exercise 4a
playerChoice :: String -> IO String 
playerChoice prompt = do putStr prompt 
                         c <- getLine
                         if c /= [] && (c == "F" || c == "S") then return c 
                         else do putStrLn "ERROR: invalid input" 
                                 playerChoice prompt
                  
type Grid = [[Player]]

data Player = O | B | X deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool 
full = notElem B . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where 
          os = length (filter (==O) ps)
          xs = length (filter (==X) ps)
          ps = concat g

--added for exercise 4b
sublists :: [Player] -> [[Player]]
sublists xs = [take winLine (drop n xs) | n <- [0..size-winLine]]

--modified for exercise 4b
wins :: Player -> Grid -> Bool 
wins p g = any line (concatMap sublists (rows ++ cols ++ dias))
           where
            line = all (==p)
            rows = g
            cols = transpose g
            dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = 
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where 
            beside = foldr1 (zipWith (++))
            bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = 
  [chop size (xs ++ [p] ++ ys) | valid g i]
  where (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs) 

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else 
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

--utility functions from Game of life
tictactoe :: IO ()
tictactoe = run empty O

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
-------------------------------------

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g 
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins!\n"
         | wins X g  = putStrLn "Player X wins!\n"
         | full g    = putStrLn "It's a draw!\n"
         | otherwise =
              do i <- getNat (prompt p)
                 case move g i p of
                    [] -> do putStrLn "ERROR: Invalid move"
                             run' g p 
                    [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
   | won g  = []
   | full g = []
   | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimax (Node g ts)
   | turn g == O = Node (g, minimum ps) ts' 
   | turn g == X = Node (g, maximum ps) ts' 
                   where
                     ts' = map minimax ts
                     ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid 
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where
                  tree = prune depth (gametree g p)
                  Node (_, best) ts = minimax tree 

--added for exercise 2
bestmoves :: Grid -> Player -> [Grid] 
bestmoves g p = [g' | Node (g',p') _ <- ts, p' == best]
               where
                  tree = prune depth (gametree g p)
                  Node (_, best) ts = minimax tree 

--added for exercise 3
fastBestmove :: Grid -> Player -> Grid 
fastBestmove g p = head [g'| Node (g',p') _ <- sortOn getDepth ts, p' == best]
               where
                  tree = prune depth (gametree g p)
                  Node (_, best) ts = minimax tree 

--added for exercise 4c
searchBestmove :: Grid -> Player -> Tree Grid -> Grid
searchBestmove g p (Node g' _) | g == g' = bestmove g p 
searchBestmove g p (Node g' _) | g /= g' = [[]]
searchBestmove g p (

--modified for exercise 4c
play :: Grid -> Player -> Tree Grid -> IO ()
play g p t = do cls
                goto (1,1)
                putGrid g 
                play' g p t

--modified for exercise 4c
play' :: Grid -> Player -> Tree Grid -> IO ()
play' g p t  | wins O g  = putStrLn "Player O wins!\n"
             | wins X g  = putStrLn "Player X wins!\n"
             | full g    = putStrLn "It's a draw!\n"
             | p == O    = do i <- getNat (prompt p) 
                              case move g i p of
                                 [] -> do putStrLn "ERROR: Invalid move"
                                          play' g p t
                                 [g'] -> play g' (next p) t
             | p == X    = do putStr "Player X is thinking... "
                              --added for exercise 2
                              {-
                              let gs = bestmoves g p
                              n <- randomRIO (0, length gs - 1)
                              play (gs !! n) (next p)
                              -}
                              --(play $! (bestmove g p)) (next p)
                              --added for exercise 3
                              (play $! (fastBestmove g p)) (next p) t

--added for exercise 1
getDepth :: Tree a -> Int
getDepth (Node _ []) = 0
getDepth (Node _ ts) = maximum (map getDepth ts) + 1  

numNodes :: Tree a -> Int
numNodes (Node _ []) = 1
numNodes (Node _ ts) = sum (map numNodes ts) + 1   
