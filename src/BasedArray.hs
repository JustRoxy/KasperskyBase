module BasedArray where

import BaseCrypto
import Control.Applicative
import qualified Data.Vector as V
data ArrayContent = B Base | Str String deriving (Show, Eq)

data Base = Base64 String | Base32 String | Base16 String | Base64Url String deriving (Show, Eq)

base :: Base -> Either Error String
base = getResult
  where 
    getResult (Base64 x) = decode64 x
    getResult (Base32 x) = decode32 x
    getResult (Base16 x) = decode16 x
    getResult (Base64Url x) = decodeUrlSafe x

conv :: ArrayContent -> Either Error String
conv (B bs) = base bs
conv (Str v) = decode16 v <|> decode32F v <|> decode64 v <|> decodeUrlSafe v
  where decode32F = \x -> if (length $ filter (== '=') x) == 1 
                          then Left "Not 32 encoding" 
                          else decode32 x

fromList :: [ArrayContent] -> Either Error (V.Vector String)
fromList = toVector . mapM conv
  where 
    toVector (Left er) = Left er
    toVector (Right vals) = Right $ V.fromList vals 

slice = V.slice