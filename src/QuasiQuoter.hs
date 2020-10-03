{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module QuasiQuoter
  ( base64,
    base32,
    base16,
    runBase,
  )
where

import BaseCrypto (Error, decode16, decode32, decode64)
import Data.Char (isAlpha, isDigit)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

baser :: (String -> TH.ExpQ) -> QuasiQuoter
baser x =
  QuasiQuoter
    { quoteExp = x,
      quotePat = undefined,
      quoteDec = undefined,
      quoteType = undefined
    }

base64 :: QuasiQuoter
base64 = baser (\s -> [|runBase decode64 s|])

base32 :: QuasiQuoter
base32 = baser (\s -> [|runBase decode32 s|])

base16 :: QuasiQuoter
base16 = baser (\s -> [|runBase decode16 s|])

runBase :: (String -> Either Error String) -> String -> String
runBase f v = case f $ filter (\x -> isAlpha x || isDigit x || x `elem` "=_-") v of
  Right a -> a
  Left a -> "Error: " ++ a