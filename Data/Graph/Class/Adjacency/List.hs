{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.Adjacency.List
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs, fundeps
--
----------------------------------------------------------------------------

module Data.Graph.Class.Adjacency.List
  ( AdjacencyListGraph(..)
  , defaultOutEdges
  , module Data.Graph.Class
  ) where

import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Graph.Class

defaultOutEdges :: AdjacencyListGraph g v e => v -> g [(v, v)]
defaultOutEdges v = liftM (map ((,) v)) (adjacentVertices v)

-- | Minimal definition: 'source', 'target', and either 'adjacentVertices' with @'outEdges' = 'defaultOutEdges'@ or 'outEdges'
class Graph g v e => AdjacencyListGraph g v e | g -> v e where
  -- /O(1)/
  source :: e -> g v
  -- /O(1)/
  target :: e -> g v
  -- /O(e)/ in the number of out edges
  outEdges :: v -> g [e]

  -- /O(e)/
  outDegree :: v -> g Int
  outDegree v = liftM length (outEdges v)

  adjacentVertices :: v -> g [v]
  adjacentVertices = outEdges >=> mapM target

instance AdjacencyListGraph g v e => AdjacencyListGraph (Strict.StateT s g) v e where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance AdjacencyListGraph g v e => AdjacencyListGraph (Lazy.StateT s g) v e where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance (AdjacencyListGraph g v e, Monoid m) => AdjacencyListGraph (Strict.WriterT m g) v e where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance (AdjacencyListGraph g v e, Monoid m) => AdjacencyListGraph (Lazy.WriterT m g) v e where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree
