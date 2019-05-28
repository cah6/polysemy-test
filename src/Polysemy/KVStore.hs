{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Polysemy.KVStore
  ( -- * Effect
    KVStore (..)

    -- * Actions
  , lookupKV
  -- , lookupAllKV
  , writeKV
  --, deleteKV
  , updateKV

    -- * Interpretations
  , runKVStoreAsState
  , runKVStorePurely
  ) where

import qualified Data.Map as M
import           Polysemy
import           Polysemy.State

------------------------------------------------------------------------------
-- | Models things like Redis, HTTP GET/POST, etc. Things that are keyed, have
-- a value, and may or may not be there.
data KVStore k v m a where
  LookupKV :: k -> KVStore k v m (Maybe v)
  LookupAllKV :: KVStore k v m [v]
  UpdateKV :: k -> Maybe v -> KVStore k v m ()

makeSem ''KVStore

writeKV :: Member (KVStore k v) r => k -> v -> Sem r ()
writeKV k = updateKV k . Just
{-# INLINE writeKV #-}

deleteKV :: forall k v r. Member (KVStore k v) r => k -> Sem r ()
deleteKV k = updateKV k (Nothing @v)
{-# INLINE deleteKV #-}


runKVStoreAsState :: Ord k => Sem (KVStore k v ': r) a -> Sem (State (M.Map k v) ': r) a
runKVStoreAsState = reinterpret $ \case
  LookupKV k    -> gets $ M.lookup k
  LookupAllKV   -> gets $ M.elems
  UpdateKV k v  -> modify $ M.alter (const v) k
{-# INLINE runKVStoreAsState #-}


runKVStorePurely
    :: Ord k
    => M.Map k v
    -> Sem (KVStore k v ': r) a
    -> Sem r (M.Map k v, a)
runKVStorePurely m = runState m . runKVStoreAsState
{-# INLINE runKVStorePurely #-}