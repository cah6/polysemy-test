{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ServantExample where

program :: Members '[KVStore UUID MyData, Random] r => Sem r String
program = do
  newId :: UUID <- random
  updateKV newId $ Just (MyData "s1" 1)
  d :: Maybe MyData <- lookupKV newId
  return $ "Value was: " <> show d

programInMemory :: Sem '[] (R.StdGen, (Map UUID MyData, String))
programInMemory = runRandom (R.mkStdGen 1) . runKVStorePurely empty $ program

programInElasticsearch :: Sem '[Lift IO] (Either ElasticsearchKVError String)
programInElasticsearch = (runErrorInIO runM
  . provideBhLocalhost
  . runKVStoreBloodhound
  . runRandomIO)
  program

main :: IO ()
main = runM programInElasticsearch >> return ()