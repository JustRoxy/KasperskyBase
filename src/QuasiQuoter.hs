{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module QuasiQuoter
    ( base64
    , base32
    , base16
    ) where

import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote
import           BaseCrypto
import           Data.Char (isAlpha, isDigit)
import           Control.Applicative

baser :: (String -> TH.ExpQ) -> QuasiQuoter
baser x = QuasiQuoter { quoteExp = x,
                        quotePat = undefined,
                        quoteDec = undefined,
                        quoteType = undefined
        }

base64 = baser (\s -> [|runBase decode64 s|])
base32 = baser (\s -> [|runBase decode32 s|])
base16 = baser (\s -> [|runBase decode16 s|])

runBase :: (String -> Either Error String) -> String -> String
runBase f x = case f $ filter (\x -> isAlpha x || isDigit x || x `elem` "=_-") x of
              Right x -> x
              Left x -> "Error: " ++ x


