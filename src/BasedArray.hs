{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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

import qualified Data.ByteString as BS
--               Well, there was a Storable vector after all. Idk how did i missed that
import qualified Data.ByteString.Internal as BS
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
import Foreign.Storable (Storable (sizeOf))
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (Lit (IntegerL))
import QuasiQuoter (base64)

sizeOfElem :: (Storable a) => VS.Vector a -> Int
sizeOfElem = sizeOf . VS.head

-- >>> sizeOfElem (VS.fromList [1,2,3] :: VS.Vector Word8)
-- 1

byteStringToVector :: BS.ByteString -> Allocated
byteStringToVector bs = vec
  where
    vec = VS.unsafeFromForeignPtr (castForeignPtr ptr) (align off) (align len) -- Generates VS.Vector from (pointer) (alligned offset) (alligned length)
    (ptr, off, len) = BS.toForeignPtr bs
    align = flip div $ sizeOfElem vec -- X `div` sizeOfElem vec

vectorToByteString :: Allocated -> BS.ByteString
vectorToByteString vec =
  let (ptr, off, len) = VS.unsafeToForeignPtr vec
   in BS.fromForeignPtr (castForeignPtr ptr) (align off) (align len)
  where
    align = (* sizeOfElem vec)

type Allocated = VS.Vector Word8

packVec :: BS.ByteString -> Allocated
packVec = byteStringToVector

unpackVec :: Allocated -> BS.ByteString
unpackVec = vectorToByteString

example :: Allocated
example = packVec [base64|ZXhhbXBsZQ==|]

-- | The result of this function looks like (Memory pointer, start, offset):(0x000000000730d2a0,0,7)
vectorPointer :: Allocated -> (ForeignPtr Word8, Int, Int)
vectorPointer = VS.unsafeToForeignPtr

-- | The function takes `ForeignPtr Word8`, start and offset, and recreates StorableVector of `Word8`.
recreateVector :: ForeignPtr Word8 -> Int -> Int -> Allocated
recreateVector = VS.unsafeFromForeignPtr

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