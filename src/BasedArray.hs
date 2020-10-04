{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module BasedArray
  ( packVec,
    unpackVec,
    example,
    mkSlice,
    vectorPointer,
    recreateVector,
  )
where

import Data.ByteString (ByteString, pack, unpack)
--               Well, there was a Storable vector after all. Idk how did i missed that

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

type Allocated = VS.Vector Word8

packVec :: ByteString -> Allocated
packVec = VS.fromList . unpack

unpackVec :: Allocated -> ByteString
unpackVec = pack . VS.toList -- Vector [1,0] -> [1,0] -> "10"

example :: Allocated
example = packVec [base64|ZXhhbXBsZQ==|]

-- | The result of this function looks like (Memory pointer, start, offset):(0x000000000730d2a0,0,7)
vectorPointer :: Allocated -> (ForeignPtr Word8, Int, Int)
vectorPointer = unsafeToForeignPtr

-- |The function takes `ForeignPtr`, start and offset, and recreates StorableVector of `a`, where `a` should be `Storable`.
recreateVector :: ForeignPtr Word8 -> Int -> Int -> Allocated
recreateVector = unsafeFromForeignPtr

slice :: Int -> Int -> Allocated -> Allocated
slice = VS.slice

mkSlice :: Integer -> Integer -> TH.Name -> TH.ExpQ
mkSlice x y s =
  apply
    ( apply
        ( apply
            (variable $ TH.mkName "slice") -- applying x to slice (slice x)
            (intVar x)
        )
        (intVar y) -- applying y to slice x (slice x y)
    )
    (variable s) -- applying s to slice x y (slice x y s)
  where
    apply = TH.appE
    intVar = TH.litE . IntegerL
    variable = TH.varE