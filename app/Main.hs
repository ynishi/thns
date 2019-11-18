module Main where

import           Data.Either
import           Lib

main :: IO ()
main = do
  contents <- getContents
  run parseExpr contents
