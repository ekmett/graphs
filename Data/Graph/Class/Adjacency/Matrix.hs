{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.Adjacency.Matrix
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Class.Adjacency.Matrix 
  ( AdjacencyMatrixGraph(..)
  , module Data.Graph.Class
  ) where

import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Graph.Class

class Graph g => AdjacencyMatrixGraph g where
  edge :: Vertex g -> Vertex g -> g (Maybe (Edge g))

instance AdjacencyMatrixGraph g => AdjacencyMatrixGraph (Strict.StateT s g) where
  edge a b = lift (edge a b)

instance AdjacencyMatrixGraph g => AdjacencyMatrixGraph (Lazy.StateT s g) where
  edge a b = lift (edge a b)

instance (AdjacencyMatrixGraph g, Monoid m) => AdjacencyMatrixGraph (Strict.WriterT m g) where
  edge a b = lift (edge a b)

instance (AdjacencyMatrixGraph g, Monoid m) => AdjacencyMatrixGraph (Lazy.WriterT m g) where
  edge a b = lift (edge a b)

