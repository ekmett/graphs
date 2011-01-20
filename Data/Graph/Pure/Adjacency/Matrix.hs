{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Pure.Adjacency.Matrix
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Pure.Adjacency.Matrix
  ( Matrix(..)
  ) where

import Data.Ix
import Data.Array.IArray
import Data.Graph.PropertyMap
import Data.Graph.Pure.Class
import Data.Graph.Pure.Class.AdjacencyMatrix

newtype Matrix a i = Matrix { getMatrix :: a (i,i) Bool } 

instance (IArray a Bool, Ix i, Ord i) => Graph (Matrix a i) where
  type Vertex (Matrix a i) = i
  vertexMap = propertyMap

instance (IArray a Bool, Ix i, Ord i) => EdgedGraph (Matrix a i) where
  type Edge (Matrix a i) = (i, i)

instance (IArray a Bool, Ix i, Ord i) => AdjacencyMatrix (Matrix a i) where
  edge i j (Matrix a) = 
    if inRange (bounds a) ix && (a ! ix) 
    then Just ix
    else Nothing
    where ix = (i, j)
