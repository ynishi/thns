{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM
import           Data.Either
import           Data.IntMap              (IntMap)
import qualified Data.IntMap              as IntMap
import           Lib
import qualified Network.Wai.Handler.Warp as Warp
import           Servant

main :: IO ()
main = cmd

cmd = do
  contents <- getContents
  run parseExpr contents
