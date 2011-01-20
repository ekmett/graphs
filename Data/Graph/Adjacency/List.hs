{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Adjacency.List
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Adjacency.List
  ( Table(..)
  , ask
  ) where

import Control.Applicative
import Data.Ix
import Data.Array
import Data.Graph.PropertyMap
import Data.Graph.Class
import Data.Graph.Class.Adjacency
import Data.Graph.Class.Incidence

newtype Table i a = Table { getTable :: Array i [i] -> a }  

ask :: Table i (Array i [i])
ask = Table id

instance Functor (Table i) where
  fmap f (Table g) = Table (f . g)
  b <$ _ = pure b

instance Applicative (Table i) where
  pure = Table . const
  Table f <*> Table a = Table $ \t -> f t (a t)

instance Monad (Table i) where
  return = Table . const
  Table f >>= k = Table $ \t -> getTable (k (f t)) t

instance Ord i => Graph (Table i) where
  type Vertex (Table i) = i
  vertexMap = propertyMap

instance (Ix i, Ord i) => AdjacencyGraph (Table i) where
  adjacentVertices v = Table $ \g -> if inRange (bounds g) v 
                                     then g ! v 
                                     else []

instance Ord i => EdgedGraph (Table i) where
  type Edge (Table i) = (i, i)

instance (Ix i, Ord i) => IncidenceGraph (Table i) where
  source (a, _) = pure a
  target (_, b) = pure b
  outEdges v = map ((,) v) <$> adjacentVertices v
