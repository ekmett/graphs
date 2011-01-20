{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.Bidirectional
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs, fundeps
--
----------------------------------------------------------------------------

module Data.Graph.Class.Bidirectional 
  ( BidirectionalGraph(..)
  , module Data.Graph.Class.Adjacency.List
  ) where

import Control.Monad
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Graph.Class.Adjacency.List

class AdjacencyListGraph g v e => BidirectionalGraph g v e | g -> v e where
  -- /O(e)/
  inEdges :: v -> g [e]
  -- /O(e)/
  inDegree :: v -> g Int
  inDegree v = length `liftM` inEdges v
  
  incidentEdges :: v -> g [e]
  incidentEdges v = liftM2 (++) (inEdges v) (outEdges v)

  degree :: v -> g Int
  degree v = liftM2 (+) (inDegree v) (outDegree v)

instance BidirectionalGraph g v e => BidirectionalGraph (Strict.StateT s g) v e where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance BidirectionalGraph g v e => BidirectionalGraph (Lazy.StateT s g) v e where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance (BidirectionalGraph g v e, Monoid m) => BidirectionalGraph (Strict.WriterT m g) v e where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance (BidirectionalGraph g v e, Monoid m) => BidirectionalGraph (Lazy.WriterT m g) v e where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree
