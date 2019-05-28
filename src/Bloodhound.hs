{-# LANGUAGE RecordWildCards #-}
module Bloodhound where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Database.V5.Bloodhound.Client
import Database.V5.Bloodhound.Types hiding (_id)
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (responseBody)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith, tlsManagerSettings)
import System.Environment

import Polysemy
import Polysemy.Error
import Polysemy.KVStore
import Polysemy.Input
import Polysemy.Reader

esIndex = IndexName "index1"
esMapping = MappingName "mapping1"

data ElasticsearchKVError = 
    ResponseParseError Text
  | KnownEsError Text

provideBhEnv :: Members '[Lift IO] r
  => Sem (Input BHEnv ': r) a
  -> Sem r a
provideBhEnv = interpret $ \case 
  Input -> do
    manager <- sendM $ newTlsManagerWith tlsSettings
    espw <- pack <$> (sendM $ getEnv "ES_PASSWORD")
    return $ mkBHEnv (Server "https://d645aa815ce645b695844914a1ccb37f.us-east-1.aws.found.io:9243/") manager
      where
    addAuth pw env = env { bhRequestHook = basicAuthHook (EsUsername "elastic") (EsPassword pw) }
    tlsSettings = mkManagerSettings (TLSSettingsSimple False True True) Nothing

provideBhLocalhost :: Members '[Lift IO] r
  => Sem (Input BHEnv ': r) a
  -> Sem r a
provideBhLocalhost = interpret $ \case 
  Input -> do
    manager <- sendM $ newTlsManagerWith tlsSettings
    return $ mkBHEnv (Server "http://localhost:9200") manager
      where
    tlsSettings = mkManagerSettings (TLSSettingsSimple False True True) Nothing

runKVStoreBloodhound :: (Show k, FromJSON v, ToJSON v)
  => Members '[Input BHEnv, Lift IO, Error ElasticsearchKVError] r
  => Sem (KVStore k v ': r) a 
  -> Sem r a
runKVStoreBloodhound = interpret $ \case
  LookupKV k    -> do
    bhEnv <- input 
    reply <- runBH bhEnv $ getDocument esIndex esMapping (mkDocId k)
    parseSingleDocResponse reply
  UpdateKV k v  -> do 
    bhEnv <- input
    _ <- runBH bhEnv $ indexDocument esIndex esMapping defaultIndexDocumentSettings v (mkDocId k)
    return ()

mkDocId :: Show k => k -> DocId
mkDocId = DocId . pack . show

parseSingleDocResponse :: FromJSON a
  => Members '[Error ElasticsearchKVError] r
  => Reply
  -> Sem r (Maybe a)
parseSingleDocResponse reply = case eitherDecode (responseBody reply) of 
  Left err          -> throw $ ResponseParseError (pack err)
  Right esResponse  -> case esResponse of
    MyEsError EsError{..}   -> throw $ KnownEsError errorMessage
    EsSuccess EsResult{..}  -> case foundResult of 
      Nothing                           -> return Nothing
      Just (EsResultFound version doc)  -> return $ Just doc

-- | Data type that represents what Elasticsearch will return: either what you
-- requested, or some formatted error. Mostly exists to attach FromJSON. 
data EsResponse a = EsSuccess a | MyEsError EsError

instance FromJSON a => FromJSON (EsResponse a) where 
  parseJSON v = (EsSuccess <$> parseJSON v) <|> (MyEsError <$> parseJSON v)