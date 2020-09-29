{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module BasedArray
    ( fromString
    , example
    , mkSlice
    , vectorPointer
    , recreateVector
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
  deriving (Eq, Show, Storable)

fromString :: String -> VS.Vector VectorContent
fromString x = VS.fromList (Prelude.map (VectorContent . fromIntegral . ord) x)

example :: VS.Vector VectorContent
example = fromString [base64|ZXhhbXBsZQ==|]

-- | The result of this function looks like (Memory pointer, start, offset):(0x000000000730d2a0,0,7)
vectorPointer :: (Storable a) => VS.Vector a -> (ForeignPtr a, Int, Int)
vectorPointer = unsafeToForeignPtr
-- | The function takes ForeignPtr, start and offset, and recreates StorableVector of a, where a should be Storable
-- | let (p, s, o) = vectorPointer x
-- | recreateVector p s o == x
recreateVector :: (Storable a) => ForeignPtr a -> Int -> Int -> Vector a
recreateVector = unsafeFromForeignPtr

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
