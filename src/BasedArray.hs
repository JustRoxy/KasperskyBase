{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module BasedArray
    ( fromString
    , example
    , mkSlice
    ) where

import           BaseCrypto
import           Control.Applicative
import qualified Data.Vector         as V
import           Data.Vector.Storable.Mutable (Storable)
import           QuasiQuoter
import           Data.Char (ord)
import           Data.Word (Word8)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax

newtype VectorContent = VectorContent Word8
  deriving (Show, Storable)

fromString :: String -> V.Vector VectorContent
fromString x = V.fromList (map (VectorContent . fromIntegral . ord) x)

example :: V.Vector VectorContent
example = fromString [base64|ZXhhbXBsZQ==|]

slice = V.slice

mkSlice :: Integer -> Integer -> TH.Name -> TH.ExpQ
mkSlice x y s= apply
                  (apply
                    (apply
                      (variable $ TH.mkName "slice") --applying x to slice (slice x)
                      (intVar x)
                    )
                    (intVar y)) --applying y to slice x (slice x y)
                  (variable s) --applying s to slice x y (slice x y s)
    where
      apply x y = TH.appE x y
      intVar = TH.litE . IntegerL
      variable = TH.varE
