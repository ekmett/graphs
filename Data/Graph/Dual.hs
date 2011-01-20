{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Dual
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs, fundeps
--
----------------------------------------------------------------------------

module Data.Graph.Dual
  ( Dual(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Graph.PropertyMap
import Data.Graph.Class.Adjacency.Matrix
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

instance Graph g v e => Graph (Dual g) v e where
  vertexMap = Dual . liftM liftPropertyMap . vertexMap
  edgeMap   = Dual . liftM liftPropertyMap . edgeMap

instance AdjacencyMatrixGraph g v e => AdjacencyMatrixGraph (Dual g) v e where
  edge l r = Dual (edge r l)

instance BidirectionalGraph g v e => AdjacencyListGraph (Dual g) v e where
  source = Dual . target
  target = Dual . source
  outEdges = Dual . inEdges
  outDegree = Dual . inDegree

instance BidirectionalGraph g v e => BidirectionalGraph (Dual g) v e where
  inEdges = Dual . outEdges
  inDegree = Dual . inDegree
  incidentEdges = Dual . incidentEdges
  degree = Dual . degree
