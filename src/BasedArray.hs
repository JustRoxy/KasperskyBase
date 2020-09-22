{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module BasedArray where

import           BaseCrypto
import           Control.Applicative
import qualified Data.Vector         as V
import Data.Vector.Storable.Mutable (Storable)
import QuasiQuoter
import Data.Char (ord)
import Data.Word (Word8)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax

newtype VectorContent = VectorContent Word8
  deriving (Show, Storable)

fromString :: String -> V.Vector VectorContent
fromString x = V.fromList (map (VectorContent . fromIntegral . ord) x)

example :: V.Vector VectorContent
example = fromString [base64|ZXhhbXBsZQ==|]

slice = V.slice

mkSlice :: Integer -> Integer -> TH.Name -> TH.ExpQ
mkSlice x y s= TH.appE
                  (TH.appE
                        (TH.appE
                          (TH.varE $ TH.mkName "slice")
                          (TH.litE (IntegerL x))
                        )
                        (TH.litE (IntegerL y)))
               (TH.varE s)