module Main where

import Control.Monad(forever)
import Lib

main :: IO ()
main = forever $ do 
    input <- getLine
    either putStrLn putStrLn (encode64 input)
