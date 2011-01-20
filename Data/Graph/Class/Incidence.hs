{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.Incidence
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MTPCs, fundeps
--
----------------------------------------------------------------------------

module Data.Graph.Class.Incidence where

import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Graph.Class.Adjacency
import Data.Graph.Class

defaultAdjacentVertices :: IncidenceGraph g => Vertex g -> g [Vertex g]
defaultAdjacentVertices = outEdges >=> mapM target

class AdjacencyGraph g => IncidenceGraph g where
  type Edge g
  -- /O(1)/
  source :: Edge g -> g (Vertex g)
  -- /O(1)/
  target :: Edge g -> g (Vertex g)
  -- /O(e)/ in the number of out edges
  outEdges :: Vertex g -> g [Edge g]
  -- /O(e)/
  outDegree :: Vertex g -> g Int
  outDegree v = liftM length (outEdges v)

instance IncidenceGraph g => IncidenceGraph (Strict.StateT s g) where
  type Edge (Strict.StateT s g) = Edge g
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance IncidenceGraph g => IncidenceGraph (Lazy.StateT s g) where
  type Edge (Lazy.StateT s g) = Edge g
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance (IncidenceGraph g, Monoid m) => IncidenceGraph (Strict.WriterT m g) where
  type Edge (Strict.WriterT m g) = Edge g
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance (IncidenceGraph g, Monoid m) => IncidenceGraph (Lazy.WriterT m g) where
  type Edge (Lazy.WriterT m g) = Edge g
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree
