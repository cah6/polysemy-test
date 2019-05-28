{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ServantExample where

import Prelude hiding (lookup)

import Bloodhound
import Control.Monad
import Control.Monad.Except
import Data.Aeson
import Data.Maybe
import Data.UUID
import Database.V5.Bloodhound.Client
import Database.V5.Bloodhound.Types hiding (_id)
import GHC.Generics
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Logger (withStdoutLogger)
import Polysemy
import Polysemy.Error
import Polysemy.KVStore
import Polysemy.Input
import Polysemy.Random
import Polysemy.State
import Servant
import System.IO
import qualified System.Random as R

import Data.Map

data MyData = MyData
  { data1 :: String
  , data2 :: Int
  } deriving (Generic, Show, FromJSON, ToJSON)

updateP :: Members '[KVStore UUID MyData, Random] r => UUID -> MyData -> Sem r NoContent
updateP uuid myData = do 
  newId :: UUID <- random
  updateKV newId $ Just (MyData "s1" 1)
  return NoContent

lookupP :: Members '[KVStore UUID MyData] r => UUID -> Sem r MyData
lookupP k = do 
  d :: Maybe MyData <- lookupKV k
  return $ fromJust d

type MyDataApi =
      "happy-hours" :> Capture "happy-hour-id" UUID :> ReqBody '[JSON] MyData :> Put '[JSON] NoContent
  :<|>"happy-hours" :> Capture "happy-hour-id" UUID :> Get '[JSON] MyData

server :: Members '[KVStore UUID MyData, Random] r => ServerT MyDataApi (Sem r)
server = updateP :<|> lookupP

type MyApplication a = Sem '[Random, KVStore UUID MyData, Input BHEnv, Error ElasticsearchKVError, Error ServantErr, Lift IO] a

programInServant ::  MyApplication a 
  ->  Sem '[Lift IO] (Either ServantErr a)
programInServant input = (runError
  . runErrorAsAnother toServantErr
  . provideBhLocalhost
  . runKVStoreBloodhound @UUID @MyData
  . runRandomIO) 
  input

-- natural transformation from MyApplication to Servant's Handler
nt :: MyApplication a -> Handler a
nt input = Handler $ ExceptT $ runM $ programInServant input 

toServantErr :: ElasticsearchKVError -> ServantErr
toServantErr = \case 
  ResponseParseError text -> err500
  KnownEsError text       -> err500

runServant :: IO ()
runServant = do
  withStdoutLogger $ \apacheLogger -> do
    let port = 3000
        mainLoopF = hPutStrLn stderr ("listening on port " ++ show port)
        settings = (setPort port . setBeforeMainLoop mainLoopF . setLogger apacheLogger) defaultSettings
    runSettings settings mkApp

mkApp :: Application
mkApp = serve myDataApi serverDefinition
    where
  serverDefinition = hoistServer myDataApi nt server

myDataApi :: Proxy MyDataApi
myDataApi = Proxy
