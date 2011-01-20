{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.Adjacency.Matrix
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs, fundeps
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

class Graph g v e => AdjacencyMatrixGraph g v e | g -> v e where
  edge :: v -> v -> g (Maybe e)

instance AdjacencyMatrixGraph g v e => AdjacencyMatrixGraph (Strict.StateT s g) v e where
  edge a b = lift (edge a b)

instance AdjacencyMatrixGraph g v e => AdjacencyMatrixGraph (Lazy.StateT s g) v e where
  edge a b = lift (edge a b)

instance (AdjacencyMatrixGraph g v e, Monoid m) => AdjacencyMatrixGraph (Strict.WriterT m g) v e where
  edge a b = lift (edge a b)

instance (AdjacencyMatrixGraph g v e, Monoid m) => AdjacencyMatrixGraph (Lazy.WriterT m g) v e where
  edge a b = lift (edge a b)
