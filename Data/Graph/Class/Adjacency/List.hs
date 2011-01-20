{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.Adjacency.List
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
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

defaultOutEdges :: AdjacencyListGraph g => Vertex g -> g [(Vertex g, Vertex g)]
defaultOutEdges v = liftM (map ((,) v)) (adjacentVertices v)

-- | Minimal definition: 'source', 'target', and either 'adjacentVertices' with @'outEdges' = 'defaultOutEdges'@ or 'outEdges'
class Graph g => AdjacencyListGraph g where
  -- /O(1)/
  source :: Edge g -> g (Vertex g)
  -- /O(1)/
  target :: Edge g -> g (Vertex g)
  -- /O(e)/ in the number of out edges
  outEdges :: Vertex g -> g [Edge g]

  -- /O(e)/
  outDegree :: Vertex g -> g Int
  outDegree v = liftM length (outEdges v)

  adjacentVertices :: Vertex g -> g [Vertex g]
  adjacentVertices = outEdges >=> mapM target

instance AdjacencyListGraph g => AdjacencyListGraph (Strict.StateT s g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance AdjacencyListGraph g => AdjacencyListGraph (Lazy.StateT s g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance (AdjacencyListGraph g, Monoid m) => AdjacencyListGraph (Strict.WriterT m g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance (AdjacencyListGraph g, Monoid m) => AdjacencyListGraph (Lazy.WriterT m g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree
