{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Adjacency.Matrix
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Adjacency.Matrix
  ( Matrix(..)
  ) where

import Control.Applicative
import Data.Ix
import Data.Functor
import Data.Array.IArray
import Data.Graph.PropertyMap
import Data.Graph.Class
import Data.Graph.Class.AdjacencyMatrix

newtype Matrix arr i a = Matrix { getMatrix :: arr (i,i) Bool -> a } 

instance Functor (Matrix arr i) where
  fmap f (Matrix g) = Matrix (f . g)
  b <$ _ = pure b

instance Applicative (Matrix arr i) where
  pure = Matrix . const
  Matrix f <*> Matrix a = Matrix $ \t -> f t (a t)

instance Monad (Matrix arr i) where
  return = Matrix . const
  Matrix f >>= k = Matrix $ \t -> getMatrix (k (f t)) t

instance Ord i => Graph (Matrix arr i) where
  type Vertex (Matrix arr i) = i
  vertexMap = propertyMap

instance Ord i => EdgedGraph (Matrix arr i) where
  type Edge (Matrix arr i) = (i, i)

instance (IArray arr Bool, Ix i, Ord i) => AdjacencyMatrix (Matrix arr i) where
  edge i j = Matrix $ \a ->
    if inRange (bounds a) ix && (a ! ix) 
    then Just ix
    else Nothing
    where ix = (i, j)
