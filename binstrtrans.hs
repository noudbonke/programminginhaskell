import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0              

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

--added for exercise 7
parity :: [Bit] -> Bit
parity bits = (sum bits) `mod` 2

--added for exercise 7
addParity :: [Bit] -> [Bit]
addParity bits = parity bits : bits 

--added for exercise 7
checkParity :: [Bit] -> [Bit]
checkParity bits | head bits == parity (tail bits) = tail bits
                 | otherwise = error "at least one bit corrupted during transmission"

--modified for exercise 7
encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

--modified for exercise 7
chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

--added for exercise 8
faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

faultyTransmit :: String -> String 
faultyTransmit = decode . faultyChannel . encode

