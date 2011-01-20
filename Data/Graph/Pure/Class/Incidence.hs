{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.Pure.Incidence
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Pure.Class.Incidence 
  ( IncidenceGraph(..)
  , defaultAdjacentVertices
  , module Data.Graph.Pure.Class.Edged
  , module Data.Graph.Pure.Class.Adjacency
  ) where

import Control.Monad
import Data.Graph.Pure.Class.Adjacency (AdjacencyGraph(..))
import Data.Graph.Pure.Class.Edged

defaultAdjacentVertices :: IncidenceGraph g => Vertex g -> g -> [Vertex g]
defaultAdjacentVertices = outEdges >=> mapM target

class (EdgedGraph g, AdjacencyGraph g) => IncidenceGraph g where
  -- /O(1)/
  source :: Edge g -> g -> Vertex g
  -- /O(1)/
  target :: Edge g -> g -> Vertex g
  -- /O(e)/ in the number of out edges
  outEdges :: Vertex g -> g -> [Edge g]
  -- /O(e)/
  outDegree :: Vertex g -> g -> Int
  outDegree v = liftM length (outEdges v)
