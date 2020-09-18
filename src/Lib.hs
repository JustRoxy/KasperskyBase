module Lib
    ( encode64
    , encode32
    ) where

import Data.Char
import Control.Applicative
import qualified Data.Map as M

b64 :: String
b64 = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++"+/"
b32 :: String
b32 = ['A'..'Z'] ++ ['2'..'7']

baseMap :: Int -> [Int] -> [String] 
baseMap base nums = case mapM (toNDigits base . toBits) nums of
                        Right s -> s
                        Left _ -> error "Impossible condition"

b64BaseMap :: [String]
b64BaseMap = baseMap 6 [0..63]

b32BaseMap :: [String]
b32BaseMap = baseMap 5 [0..31]
                

b64Enc :: M.Map String Char 
b64Enc = M.fromList (zip b64BaseMap b64) 

b64Dec :: M.Map Char String 
b64Dec = M.fromList (zip b64 b64BaseMap)

b32Enc :: M.Map String Char
b32Enc = M.fromList (zip b32BaseMap b32)

b32Dec :: M.Map Char String
b32Dec = M.fromList (zip b32 b32BaseMap)


type Error = String

-- | The function converts the natural integer input to binary string representation.
-- | > toBits 5 = "101"
toBits :: Int -> String
toBits 0 = ""
toBits x = toBits (x `div` 2) ++ show (x `mod` 2)

-- | Just basic binary to decimal conversion
fromBits :: Int -> Int
fromBits 0 = 0
fromBits i = 2 * fromBits (i `div` 10) + (i `mod` 10)


-- | The function converts binary string to N digit form.
toNDigits :: Int -> String -> Either Error String
toNDigits n s 
        | len > n = Left ("Unsupported character " ++ [chr $ fromBits (read s :: Int)] ++ " with binary form " ++ s)
        | otherwise = Right $ replicate (n - len) '0' ++ s
        where len = length s

-- | The function converts binary string to eightfold digits form.
-- | > to8Digits 10 = 00000010
to8Digits :: String -> Either Error String
to8Digits = toNDigits 8


mapLookup :: (Ord k, Show k) => M.Map k a -> k -> Either Error [a] 
mapLookup alph c = case M.lookup c alph of 
                Just a -> Right [a]
                Nothing -> Left ("No such key in the alphabet: " ++ show c)

-- | The all-together function that encodes String c on Base base with Padding Function padding and with Alphabet alph
encode :: String -> Int -> (String -> String) -> M.Map String Char -> Either Error String
encode [] _ _ _ = Right []
encode c base padding alph = liftA2 (++) current rest
    where 
        rest = encode (drop base c) base padding alph
        current 
            | len >= base = mapLookup alph (take base c)
            | otherwise   = (++ padding c) <$> mapLookup alph lastN
            where 
                len   = length c
                lastN = c ++ replicate (base - len) '0'




encode64Padding :: String -> String
encode64Padding x 
            | len `mod` 4 == 0 = "="
            | len `mod` 2 == 0 = "=="
            | otherwise        = error "encode64Padding was invoked with {" ++ x ++ "} value"
            where len = length x

encode32Padding :: [String] -> String -> String
encode32Padding l _ = replicate (length l `mod` 5 + 1 ) '='

encode32 :: String -> Either Error String
encode32 x = mapM (to8Digits . toBits . ord) x >>= (\r -> encode (concat r) 5 (encode32Padding r) b32Enc)

encode64 :: String -> Either Error String
encode64 x =  mapM (to8Digits . toBits . ord) x >>= (\r -> encode (concat r) 6 encode64Padding b64Enc)

