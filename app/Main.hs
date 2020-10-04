
module Main where

import Control.Monad(forever)
import Data.ByteString.Base64(encodeBase64')
import qualified Data.ByteString as BS
main :: IO ()
main = forever $ do 
    input <- BS.getLine
    print (encodeBase64' $ input)
