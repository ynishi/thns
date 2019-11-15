module Main where

import           Lib

main :: IO ()
main = putStrLn $ pp c
  where
    c =
      And
        (G "tag" ((map show [1 .. 2000]) :: [String]))
        (G "tag2" (["\"a", "'b"] :: [String]))
