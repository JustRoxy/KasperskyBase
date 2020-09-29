{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module BasedArray
    ( fromString
    , example
    , mkSlice
    , vectorPointer
    ) where

import           QuasiQuoter
import           Data.Char (ord)
import           Data.Word (Word8)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax
import           Foreign.Storable
import           Foreign.ForeignPtr
--               Well, there was a Storable vector after all. Idk how did i missed that
import           Data.Vector.Storable as VS

newtype VectorContent = VectorContent Word8
  deriving (Show, Storable)

fromString :: String -> VS.Vector VectorContent
fromString x = VS.fromList (Prelude.map (VectorContent . fromIntegral . ord) x)

example :: VS.Vector VectorContent
example = fromString [base64|ZXhhbXBsZQ==|]

vectorPointer :: (Storable a) => VS.Vector a -> (ForeignPtr a, Int, Int)
vectorPointer = unsafeToForeignPtr

slice :: (Storable a) => Int -> Int -> VS.Vector a -> VS.Vector a
slice = VS.slice

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
      apply = TH.appE
      intVar = TH.litE . IntegerL
      variable = TH.varE
