{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Algorithm
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
-- Functions and data structures common to graph search algorithms
----------------------------------------------------------------------------

module Data.Graph.Algorithm
  ( GraphSearch(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Monoid

import Data.Graph.Class

-- | Graph search visitor
data GraphSearch g m = GraphSearch
  { enterVertex :: Vertex g -> g m -- called the first time a vertex is discovered
  , enterEdge   :: Edge g   -> g m -- called the first time an edge is discovered, before enterVertex
  , grayTarget  :: Edge g   -> g m -- called when we encounter a back edge to a vertex we're still processing
  , exitVertex  :: Vertex g -> g m -- called once we have processed all descendants of a vertex
  , blackTarget :: Edge g   -> g m -- called when we encounter a cross edge to a vertex we've already finished
  }

instance Graph g => Functor (GraphSearch g) where
  fmap f (GraphSearch a b c d e) = GraphSearch
    (liftM f . a)
    (liftM f . b)
    (liftM f . c)
    (liftM f . d)
    (liftM f . e)

instance Graph g => Applicative (GraphSearch g) where
  pure a = GraphSearch
    (const (return a))
    (const (return a))
    (const (return a))
    (const (return a))
    (const (return a))

  m <*> n = GraphSearch
    (\v -> enterVertex m v `ap` enterVertex n v)
    (\e -> enterEdge m e `ap`   enterEdge n e)
    (\e -> grayTarget m e `ap`  grayTarget n e)
    (\v -> exitVertex m v `ap`  exitVertex n v)
    (\e -> blackTarget m e `ap` blackTarget n e)

instance Graph g => Monad (GraphSearch g) where
  return = pure
  m >>= f = GraphSearch
    (\v -> enterVertex m v >>= ($ v) . enterVertex . f)
    (\e -> enterEdge m e >>= ($ e)   . enterEdge . f)
    (\e -> grayTarget m e >>= ($ e)  . grayTarget . f)
    (\v -> exitVertex m v >>= ($ v)  . exitVertex . f)
    (\e -> blackTarget m e >>= ($ e) . blackTarget . f)

instance (Graph g, Monoid m) => Monoid (GraphSearch g m) where
  mempty = return mempty
  mappend = liftM2 mappend
