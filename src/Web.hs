{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module Web
  ( Conv(..)
  , API
  , api
  , CRUD
  , crud
  , server
  ) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.IntMap              (IntMap)
import qualified Data.IntMap              as IntMap
import           GHC.Generics
import           Lib
import           Servant

import qualified Network.Wai.Handler.Warp as Warp

type CRUD
   = "conv" :> "all" :> Get '[ JSON] [Conv] :<|> "conv" :> ReqBody '[ JSON] Conv :> Post '[ JSON] Conv :<|> "conv" :> Capture "id" Int :> ReqBody '[ JSON] Conv :> Put '[ JSON] () :<|> "conv" :> Capture "id" Int :> Delete '[ JSON] ()

crud :: Proxy CRUD
crud = Proxy

data Conv = Conv
  { convId :: Int
  , input  :: String
  , output :: String
  , valid  :: Bool
  , err    :: String
  } deriving (Generic, FromJSON, ToJSON)

convList :: [Conv]
convList = [Conv 1 "'a b" "(a = 'b')" True ""]

type API = CRUD

api :: Proxy API
api = Proxy

server :: TVar (Int, IntMap Conv) -> Server API
server db = getConvAll :<|> postConv :<|> putConvId :<|> deleteConvId
  where
    getConvAll = liftIO $ IntMap.elems . snd <$> readTVarIO db
    postConv conv =
      liftIO . atomically $ do
        (maxId, m) <- readTVar db
        let newId = maxId + 1
            out = runE parseExpr (input conv)
            parsed =
              case out of
                Left e  -> conv {output = "", valid = False, err = e}
                Right s -> conv {output = s, valid = True, err = ""}
            newConv = parsed {convId = newId}
        writeTVar db (newId, IntMap.insert newId newConv m)
        pure newConv
    putConvId cid conv =
      liftIO . atomically . modifyTVar db $ \(maxId, m) ->
        (maxId, IntMap.insert cid conv m)
    deleteConvId cid =
      liftIO . atomically . modifyTVar db $ \(maxId, m) ->
        (maxId, IntMap.delete cid m)
--instance Arbitrary Conv where
--    arbitrary = Conv <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
