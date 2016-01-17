{-# LANGUAGE CPP, TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Dual
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Dual
  ( Dual(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.Trans.Class
import Data.Graph.PropertyMap
import Data.Graph.Class.AdjacencyList
import Data.Graph.Class.AdjacencyMatrix
import Data.Graph.Class.EdgeEnumerable
import Data.Graph.Class.VertexEnumerable
import Data.Graph.Class.Bidirectional

newtype Dual g a = Dual { runDual :: g a }

instance MonadTrans Dual where
  lift = Dual

instance Functor g => Functor (Dual g) where
  fmap f (Dual g) = Dual $ fmap f g
  b <$ Dual g = Dual $ b <$ g

instance Applicative g => Applicative (Dual g) where
  pure = Dual . pure
  Dual f <*> Dual a = Dual (f <*> a)
  Dual f <*  Dual a = Dual (f <*  a)
  Dual f  *> Dual a = Dual (f  *> a)

instance Monad g => Monad (Dual g) where
  return = Dual . return
  Dual g >>= k = Dual (g >>= runDual . k)
  Dual g >> Dual h = Dual (g >> h)

instance Graph g => Graph (Dual g) where
  type Vertex (Dual g) = Vertex g
  type Edge (Dual g) = Edge g
  vertexMap = Dual . liftM liftPropertyMap . vertexMap
  edgeMap   = Dual . liftM liftPropertyMap . edgeMap

instance AdjacencyMatrixGraph g => AdjacencyMatrixGraph (Dual g) where
  edge l r = Dual (edge r l)

instance BidirectionalGraph g => AdjacencyListGraph (Dual g) where
  source = Dual . target
  target = Dual . source
  outEdges = Dual . inEdges
  outDegree = Dual . inDegree

instance BidirectionalGraph g => BidirectionalGraph (Dual g) where
  inEdges = Dual . outEdges
  inDegree = Dual . inDegree
  incidentEdges = Dual . incidentEdges
  degree = Dual . degree

instance EdgeEnumerableGraph g => EdgeEnumerableGraph (Dual g) where
  edges = Dual edges

instance VertexEnumerableGraph g => VertexEnumerableGraph (Dual g) where
  vertices = Dual vertices
