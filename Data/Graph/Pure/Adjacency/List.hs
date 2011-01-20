{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Pure.Adjacency.List
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Pure.Adjacency.List
  ( Table(..)
  ) where

import Data.Ix
import Data.Array
import Data.Graph.PropertyMap
import Data.Graph.Pure.Class
import Data.Graph.Pure.Class.Adjacency
import Data.Graph.Pure.Class.Incidence

-- note that a Data.Graph.Graph from containers can be made into an Table Int, by just applying the constructor.

newtype Table i = Table { getTable :: Array i [i] } 

instance (Ix i, Ord i) => Graph (Table i) where
  type Vertex (Table i) = i
  vertexMap = propertyMap

instance (Ix i, Ord i) => AdjacencyGraph (Table i) where
  adjacentVertices v (Table g) 
    | inRange (bounds g) v = g ! v
    | otherwise = []

instance (Ix i, Ord i) => EdgedGraph (Table i) where
  type Edge (Table i) = (i, i)

instance (Ix i, Ord i) => IncidenceGraph (Table i) where
  source (a, _) _ = a
  target (_, b) _ = b
  outEdges v (Table g) = map ((,) v) (g ! v)
