{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module BasedArray
  ( packVec,
    unpackVec,
    example,
    mkSlice,
    vectorPointer,
    recreateVector,
  )
where

import Data.ByteString (pack, unpack)
--               Well, there was a Storable vector after all. Idk how did i missed that

import Data.Serialize (Serialize, decode, encode)
import Data.Vector.Storable as VS
  ( Vector,
    fromList,
    slice,
    toList,
    unsafeFromForeignPtr,
    unsafeToForeignPtr,
  )
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Storable (Storable)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (Lit (IntegerL))
import QuasiQuoter (base64)

newtype VectorContent = VectorContent Word8
  deriving (Eq, Show, Storable)

packVec :: Serialize a => a -> VS.Vector VectorContent
packVec x = VS.fromList (Prelude.map VectorContent (unpack $ encode x))

unpackVec :: Serialize a => VS.Vector VectorContent -> Either String a
unpackVec x = decode $ pack (Prelude.map (\(VectorContent w) -> w) $ VS.toList x) -- Vector [VectorContent 1, VectorContent 0] -> [1,0] -> "10"

example :: VS.Vector VectorContent
example = packVec [base64|ZXhhbXBsZQ==|]

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
mkSlice x y s =
  apply
    ( apply
        ( apply
            (variable $ TH.mkName "slice") --applying x to slice (slice x)
            (intVar x)
        )
        (intVar y) --applying y to slice x (slice x y)
    )
    (variable s) --applying s to slice x y (slice x y s)
  where
    apply = TH.appE
    intVar = TH.litE . IntegerL
    variable = TH.varE