{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module QuasiQuoter
  ( base64,
    base32,
    base16,
    runBase,
  )
where
import Data.Word8 ( isAlpha, isDigit )
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Data.ByteString.Base64 as B ( encodeBase64' ) 
import Data.ByteString.Base32 as B ( encodeBase32' ) 
import Data.ByteString.Base16 as B ( encodeBase16' ) 
import Data.ByteString as BS ( elem, filter, ByteString )


baser :: (String -> TH.ExpQ) -> QuasiQuoter
baser x =
  QuasiQuoter
    { quoteExp = x,
      quotePat = undefined,
      quoteDec = undefined,
      quoteType = undefined
    }

base64 :: QuasiQuoter
base64 = baser (\s -> [|runBase B.encodeBase64' s|])

base32 :: QuasiQuoter
base32 = baser (\s -> [|runBase B.encodeBase32' s|])

base16 :: QuasiQuoter
base16 = baser (\s -> [|runBase B.encodeBase16' s|])

runBase :: (BS.ByteString -> BS.ByteString) -> ByteString -> ByteString
runBase f v = f $ BS.filter (\x -> isAlpha x || isDigit x || x `BS.elem` "=_-") v