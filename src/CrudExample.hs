{-# LANGUAGE TemplateHaskell #-}
module CrudExample where

import Prelude hiding (lookup)

import Polysemy
import Polysemy.State

import Data.Map

data MyData = MyData
  { data1 :: String
  , data2 :: String
  } deriving (Show)

data CrudData m a where
  GetData :: Int -> CrudData m (Maybe MyData)
  GetAllData :: CrudData m [MyData]
  PutData :: Int -> MyData -> CrudData m ()
  DeleteData :: Int -> CrudData m ()

makeSem ''CrudData

program :: Member CrudData r => Sem r String
program = do
  -- d <- upsertData 1 (MyData "hello" "world")
  putData 1 (MyData "s1" "s2")
  d <- getData 1
  return $ "Value was: " <> show d
 
crudToState :: Sem (CrudData ': r) a -> Sem (State (Map Int MyData) ': r) a
crudToState = reinterpret \case 
  PutData id d -> do 
    current <- get
    put (current <> singleton id d)
    return ()
  GetAllData -> do 
    current :: Map Int MyData <- get
    return $ elems current
  GetData id -> do 
    current <- get
    return $ lookup id current
  DeleteData id -> do 
    current :: Map Int MyData <- get 
    _ <- put $ delete id current
    return ()

programInMemory :: Sem '[] (Map Int MyData, String)
programInMemory = (runState empty . crudToState) program

main :: IO ()
main = putStrLn (show $ run programInMemory)
