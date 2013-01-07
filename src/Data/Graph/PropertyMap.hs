-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.PropertyMap
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Total transient monadic maps, used to track information about vertices
-- and edges in a graph
----------------------------------------------------------------------------

module Data.Graph.PropertyMap 
  ( PropertyMap(..)
  , modifyP
  , intPropertyMap
  , propertyMap
  , liftPropertyMap
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

data PropertyMap m k v = PropertyMap
  { getP :: k -> m v
  , putP :: k -> v -> m (PropertyMap m k v)
  }

modifyP :: Monad m => PropertyMap m k v -> k -> (v -> v) -> m (PropertyMap m k v)
modifyP m k f = do
  a <- getP m k
  putP m k (f a)

-- A pure IntMap-backed vertex map
intPropertyMap :: Monad m => v -> PropertyMap m Int v
intPropertyMap v0 = go v0 IntMap.empty where
  go v m = PropertyMap 
    { getP = \k -> return $ maybe v id (IntMap.lookup k m)
    , putP = \k v' -> return $ go v (IntMap.insert k v' m)
    }

-- A pure Map-backed vertex map
propertyMap :: (Monad m, Ord k) => v -> PropertyMap m k v
propertyMap v0 = go v0 Map.empty where
  go v m = PropertyMap 
    { getP = \k -> return $ maybe v id (Map.lookup k m)
    , putP = \k v' -> return $ go v (Map.insert k v' m)
    }

liftPropertyMap :: (MonadTrans t, Monad m, Monad (t m)) => PropertyMap m k v -> PropertyMap (t m) k v
liftPropertyMap (PropertyMap g p) = PropertyMap (lift . g) (\k v -> liftPropertyMap `liftM` lift (p k v))

{-
-- An impure STArray-backed vertex map
stAdjVertexMap :: (DenseAdjacencyMatrix g, MonadST s g) => a -> g (PropertyMap g (Vertex g) a)
stAdjVertexMap v0 = do
  range <- vertexRange 
  arr <- newSTArray range v0
  return $ go arr
  where
    go arr = r where r = VertexMap
      { getP = readSTArray arr
      , putP = \k v -> writeSTArray arr k v >> return r
      } 
-}
