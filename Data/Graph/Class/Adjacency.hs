{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.Adjacency
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Class.Adjacency 
  ( AdjacencyGraph(..)
  , module Data.Graph.Class
  ) where

import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Graph.Class

class Graph g => AdjacencyGraph g where
  adjacentVertices :: Vertex g -> g [Vertex g]

instance AdjacencyGraph g => AdjacencyGraph (Strict.StateT s g) where
  adjacentVertices = lift . adjacentVertices

instance AdjacencyGraph g => AdjacencyGraph (Lazy.StateT s g) where
  adjacentVertices = lift . adjacentVertices

instance (AdjacencyGraph g, Monoid m) => AdjacencyGraph (Strict.WriterT m g) where
  adjacentVertices = lift . adjacentVertices

instance (AdjacencyGraph g, Monoid m) => AdjacencyGraph (Lazy.WriterT m g) where
  adjacentVertices = lift . adjacentVertices
