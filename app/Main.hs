{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.STM
import           Data.Either
import           Data.IntMap              (IntMap)
import qualified Data.IntMap              as IntMap
import           Lib
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import           Web
import Network.HTTP.Types.Header
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options (provideOptions)

main :: IO ()
main = web

web = do
  db <- atomically $ newTVar (0, IntMap.empty)
  putStrLn "port 8080"
  Warp.run 8080 $ cors (const $ Just policy) $ provideOptions crud $ serve api (server db)
  where
      policy = simpleCorsResourcePolicy
                 { corsRequestHeaders =  simpleHeaders }
