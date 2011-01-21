{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Adjacency.Matrix
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.AdjacencyMatrix
  ( AdjacencyMatrix(..)
  , ask
  ) where

import Control.Applicative
import Data.Ix
import Data.Functor
import Data.Array.IArray
import Data.Graph.PropertyMap
import Data.Graph.Class
import Data.Graph.Class.AdjacencyMatrix

newtype AdjacencyMatrix arr i a = AdjacencyMatrix { runAdjacencyMatrix :: arr (i,i) Bool -> a } 

ask :: AdjacencyMatrix arr i (arr (i, i) Bool)
ask = AdjacencyMatrix id

instance Functor (AdjacencyMatrix arr i) where
  fmap f (AdjacencyMatrix g) = AdjacencyMatrix (f . g)
  b <$ _ = pure b

instance Applicative (AdjacencyMatrix arr i) where
  pure = AdjacencyMatrix . const
  AdjacencyMatrix f <*> AdjacencyMatrix a = AdjacencyMatrix $ \t -> f t (a t)

instance Monad (AdjacencyMatrix arr i) where
  return = AdjacencyMatrix . const
  AdjacencyMatrix f >>= k = AdjacencyMatrix $ \t -> runAdjacencyMatrix (k (f t)) t

instance Ord i => Graph (AdjacencyMatrix arr i) where
  type Vertex (AdjacencyMatrix arr i) = i
  type Edge (AdjacencyMatrix arr i) = (i, i) 
  vertexMap = pure . propertyMap
  edgeMap = pure . propertyMap

instance (IArray arr Bool, Ix i) => AdjacencyMatrixGraph (AdjacencyMatrix arr i) where
  edge i j = AdjacencyMatrix $ \a ->
    if inRange (bounds a) ix && (a ! ix) 
    then Just ix
    else Nothing
    where ix = (i, j)
