{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Empty
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs, fundeps
--
----------------------------------------------------------------------------

module Data.Graph.Empty
  ( Empty(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Void
import Data.Graph.PropertyMap
import Data.Graph.Class.Adjacency.Matrix
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

instance Graph Empty Void Void where
  vertexMap _ = Empty voidMap 
  edgeMap   _ = Empty voidMap

instance AdjacencyMatrixGraph Empty Void Void where
  edge _ _ = Empty Nothing

instance AdjacencyListGraph Empty Void Void where
  source = Empty
  target = Empty
  outEdges _ = Empty []
  outDegree _ = Empty 0

instance BidirectionalGraph Empty Void Void where
  inEdges _ = Empty []
  inDegree _ = Empty 0
  incidentEdges _ = Empty []
  degree _  = Empty 0
