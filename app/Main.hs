module Main (main) where

import Lib

main :: IO ()
main = runServer "127.0.0.1" "7000"
