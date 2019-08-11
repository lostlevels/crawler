module Main where

import Lib (run)
import System.Environment

main :: IO ()
main = do
  (dbStr:seedUrl:_) <- getArgs
  run dbStr seedUrl
