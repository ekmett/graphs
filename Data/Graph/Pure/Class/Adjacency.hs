{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Pure.Class.Adjacency
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Pure.Class.Adjacency 
  ( AdjacencyGraph(..)
  , module Data.Graph.Pure.Class
  ) where

import Data.Graph.Pure.Class

class Graph g => AdjacencyGraph g where
  adjacentVertices :: Vertex g -> g -> [Vertex g]
