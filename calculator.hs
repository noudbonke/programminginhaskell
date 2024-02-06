import Control.Applicative
import Data.Char
import System.IO
import Parser
import ArithExpr

--from ch10
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

box :: [String]
box =
  [ "+---------------+"
  , "|               |"
  , "+---+---+---+---+"
  , "| q | c | d | = |"
  , "+---+---+---+---+"
  , "| 1 | 2 | 3 | + |"
  , "+---+---+---+---+"
  , "| 4 | 5 | 6 | - |"
  , "+---+---+---+---+"
  , "| 7 | 8 | 9 | * |"
  , "+---+---+---+---+"
  , "| 0 | ( | ) | / |"
  , "+---+---+---+---+"
  ]

buttons :: String
buttons = standard ++ extra
          where 
            standard = "qcd=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y, b) <- zip [1..] box]

display :: String -> IO ()
display xs = do writeat (3, 2) (replicate 13 ' ')
                writeat (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if c `elem` buttons then
                  process c xs
             else 
                  do beep
                     calc xs

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c xs | c `elem` "qQ\ESC" = quit
             | c `elem` "dD\BS\DEL" = delete xs 
             | c `elem` "=\n" = Main.eval xs
             | c `elem` "cC" = clear
             | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
            [(n, [])] -> calc (show n)
            [(n, rest)] -> do beep
                              calc (take m xs ++ ['!'] ++ drop m xs)
                                where m = length xs - length rest
            _ -> do beep
                    calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

