module Main where

import           Control.Concurrent.STM
import           Data.Either
import           Data.IntMap              (IntMap)
import qualified Data.IntMap              as IntMap
import           Lib
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import           Web

main :: IO ()
main = web

web = do
  db <- atomically $ newTVar (0, IntMap.empty)
  putStrLn "port 8080"
  Warp.run 8080 $ serve crud (server db)
