module Main where

import Lib

main :: IO ()
main = putStrLn $ pp c 
  where
    c = And (G "tag" ([1..2000]::[Int])) (G "tag2" ([1..100]::[Int]))
