{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.Bidirectional
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Class.Bidirectional 
  ( BidirectionalGraph(..)
  , module Data.Graph.Class.Incidence
  ) where

import Control.Monad
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Graph.Class.Incidence
import qualified Data.Graph.Pure.Class.Bidirectional as Pure

class IncidenceGraph g => BidirectionalGraph g where
  -- /O(n)/
  inEdges :: Vertex g -> g [Edge g]
  -- /O(n)/
  inDegree :: Vertex g -> g Int
  inDegree v = length `liftM` inEdges v
  
  incidentEdges :: Vertex g -> g [Edge g]
  incidentEdges v = liftM2 (++) (inEdges v) (outEdges v)

  degree :: Vertex g -> g Int
  degree v = liftM2 (+) (inDegree v) (outDegree v)

instance BidirectionalGraph g => BidirectionalGraph (Strict.StateT s g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance BidirectionalGraph g => BidirectionalGraph (Lazy.StateT s g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance (BidirectionalGraph g, Monoid m) => BidirectionalGraph (Strict.WriterT m g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance (BidirectionalGraph g, Monoid m) => BidirectionalGraph (Lazy.WriterT m g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance Pure.BidirectionalGraph g => BidirectionalGraph ((->) g) where
  inEdges = Pure.inEdges
  inDegree = Pure.inDegree
  incidentEdges = Pure.incidentEdges
  degree = Pure.degree
