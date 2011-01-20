{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Adjacency.List
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs, fundeps
--
----------------------------------------------------------------------------

module Data.Graph.Adjacency.List
  ( AdjacencyList(..)
  , ask
  ) where

import Control.Applicative
import Data.Ix
import Data.Array
import Data.Graph.PropertyMap
import Data.Graph.Class
import Data.Graph.Class.Adjacency.List

newtype AdjacencyList i a = AdjacencyList { runAdjacencyList :: Array i [i] -> a }  

ask :: AdjacencyList i (Array i [i])
ask = AdjacencyList id

instance Functor (AdjacencyList i) where
  fmap f (AdjacencyList g) = AdjacencyList (f . g)
  b <$ _ = pure b

instance Applicative (AdjacencyList i) where
  pure = AdjacencyList . const
  AdjacencyList f <*> AdjacencyList a = AdjacencyList $ \t -> f t (a t)

instance Monad (AdjacencyList i) where
  return = AdjacencyList . const
  AdjacencyList f >>= k = AdjacencyList $ \t -> runAdjacencyList (k (f t)) t

instance Ord i => Graph (AdjacencyList i) i (i, i) where
  vertexMap = pure . propertyMap
  edgeMap = pure . propertyMap

instance (Ix i, Ord i) => AdjacencyListGraph (AdjacencyList i) i (i, i) where
  adjacentVertices v = AdjacencyList $ \g -> if inRange (bounds g) v 
                                     then g ! v 
                                     else []
  source (a, _) = pure a
  target (_, b) = pure b
  outEdges = defaultOutEdges
