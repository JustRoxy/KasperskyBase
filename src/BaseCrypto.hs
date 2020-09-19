module BaseCrypto
    ( encode64
    , encodeUrlSafe
    , encode32
    , encode16
    , decode64
    , decodeUrlSafe
    , decode32
    , decode16
    ) where

import           Control.Applicative
import           Data.Char
import qualified Data.Map            as M

b64 :: String
b64 = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++"+/"

b64Url :: String
b64Url = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++"-_"

b32 :: String
b32 = ['A'..'Z'] ++ ['2'..'7']

b16 :: String
b16 = ['0'..'9'] ++ ['A'..'F']

baseMap :: Int -> [Int] -> [String]
baseMap base nums = case mapM (toNDigits base . toBits) nums of
  Right s -> s
  Left _  -> error "Impossible condition"


b64BaseMap :: [String]
b64BaseMap = baseMap 6 [0..63]

b32BaseMap :: [String]
b32BaseMap = baseMap 5 [0..31]

b16BaseMap :: [String]
b16BaseMap = baseMap 4 [0..15]

mMap :: Ord k => [k] -> [a] -> M.Map k a
mMap x y = M.fromList $ zip x y

b64Enc :: M.Map String Char
b64Enc = mMap b64BaseMap b64

b64Dec :: M.Map Char String
b64Dec = mMap b64 b64BaseMap

bUrlEnc :: M.Map String Char
bUrlEnc = mMap b64BaseMap b64Url

bUrlDec :: M.Map Char String
bUrlDec = mMap b64Url b64BaseMap

b32Enc :: M.Map String Char
b32Enc = mMap b32BaseMap b32

b32Dec :: M.Map Char String
b32Dec = mMap b32 b32BaseMap

b16Enc :: M.Map String Char
b16Enc = mMap b16BaseMap b16

b16Dec :: M.Map Char String
b16Dec = mMap b16 b16BaseMap

type Error = String

-- | The function converts the natural integer input to binary string representation.
-- | > toBits 5 = "101"
toBits :: (Integral a, Show a) => a -> String
toBits 0 = ""
toBits x = toBits (x `div` 2) ++ show (x `mod` 2)

-- | Just basic binary to decimal conversion
fromBits :: Integral p => p -> p
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
  Just a  -> Right [a]
  Nothing -> Left ("No such key in the alphabet: " ++ show c)

-- | The all-together function that encodes String c on Base base with Padding Function padding and with Alphabet alph
encode :: String -> Int -> (String -> String) -> M.Map String Char -> Either Error String
encode [] _ _ _ = Right []
encode c base padding alph = liftA2 (++) current rest
  where
    rest = encode (drop base c) base padding alph
    current
      | len >= base = mapLookup alph (take base c)
      | otherwise   = (++ padding c) <$> mapLookup alph (c ++ replicate (base - len) '0')
          where
            len = length c

decode :: String -> String
decode [] = []
decode c  = chr (fromBits (read (take 8 c) :: Int)) : decode (drop 8 c)

baseDecoding :: (Ord k, Show k) => [k] -> Int -> M.Map k String -> Either String String
baseDecoding [] _ _ = Right []
baseDecoding c padTrim alph = case (\r -> take (length r - padTrim) r) . concat . concat <$> mapM (mapLookup alph) c of
  Right val ->
              Right $
                if len `mod` 8 /= 0
                then decode (take (len - len `mod` 8) val)
                else decode val
                where len = length val
  Left err -> Left err


encode64Padding :: String -> String
encode64Padding x
  | ln `mod` 4 == 0 = "="
  | ln `mod` 2 == 0 = "=="
  | otherwise       = error "encode64Padding was invoked with {" ++ x ++ "} value"
    where
      ln = length x

encode32Padding :: Foldable t => t a -> p -> String
encode32Padding l _
    | ln == 1   = "======"
    | ln == 2   = "===="
    | ln == 3   = "==="
    | ln == 4   = "="
    | otherwise = ""
    where
      ln = length l `mod` 5

decode32Padding :: (Num p1, Foldable t) => t a -> p1
decode32Padding l
    | ln == 6   = 2
    | ln == 4   = 4
    | ln == 3   = 1
    | ln == 1   = 3
    | otherwise = 0
    where ln = length l

encode16Padding :: String -> String
encode16Padding _ = ""

baseEncoding :: Traversable t => Int -> (t String -> String -> String) -> M.Map String Char -> t Char -> Either Error String
baseEncoding base padding mp x = mapM (to8Digits . toBits . ord) x >>= (\r -> encode (concat r) base (padding r) mp)

encode16 :: String -> Either Error String
encode16 = baseEncoding 4 (const encode16Padding) b16Enc

encode32 :: String -> Either Error String
encode32 = baseEncoding 5 encode32Padding b32Enc

encode64 :: String -> Either Error String
encode64 = baseEncoding 6 (const encode64Padding) b64Enc

encodeUrlSafe :: String -> Either Error String
encodeUrlSafe = baseEncoding 6 (const encode64Padding) bUrlEnc

decodeUrl64 :: String -> M.Map Char String ->  Either Error String
decodeUrl64 x = let pads = length $ filter (== '=') x in baseDecoding (filter (/= '=') x) (pads * 2)

decode64 :: String -> Either Error String
decode64 x = decodeUrl64 x b64Dec

decodeUrlSafe :: String -> Either Error String
decodeUrlSafe x = decodeUrl64 x bUrlDec

decode32 :: String -> Either Error String
decode32 x = baseDecoding (filter (/= '=') x) (decode32Padding $ filter (== '=') x) b32Dec

decode16 :: String -> Either Error String
decode16 x = baseDecoding x 0 b16Dec
