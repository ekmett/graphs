{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Empty
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Empty
  ( Empty(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Void
import Data.Graph.PropertyMap
import Data.Graph.Class.AdjacencyList
import Data.Graph.Class.AdjacencyMatrix
import Data.Graph.Class.VertexEnumerable
import Data.Graph.Class.EdgeEnumerable
import Data.Graph.Class.Bidirectional

newtype Empty a = Empty { runEmpty :: a }

instance Functor Empty where
  fmap f (Empty g) = Empty $ f g
  b <$ _ = Empty b

instance Applicative Empty where
  pure = Empty 
  Empty f <*> Empty a = Empty (f a)
  a <* _ = a
  _ *> b = b

instance Monad Empty where
  return = Empty
  Empty g >>= k = k g
  _ >> b = b

voidMap :: PropertyMap Empty Void a
voidMap = PropertyMap (Empty . void) $ \_ _ -> Empty voidMap

instance Graph Empty where
  type Vertex Empty = Void
  type Edge Empty = Void
  vertexMap _ = Empty voidMap 
  edgeMap   _ = Empty voidMap

instance AdjacencyMatrixGraph Empty where
  edge _ _ = Empty Nothing

instance AdjacencyListGraph Empty where
  source = Empty
  target = Empty
  outEdges _ = Empty []
  outDegree _ = Empty 0

instance BidirectionalGraph Empty where
  inEdges _ = Empty []
  inDegree _ = Empty 0
  incidentEdges _ = Empty []
  degree _  = Empty 0

instance EdgeEnumerableGraph Empty where
  edges = return []

instance VertexEnumerableGraph Empty where
  vertices = return []
