{-# LANGUAGE TypeFamilies, ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Algorithm.BreadthFirstSearch
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
-- Breadth-first search
----------------------------------------------------------------------------

module Data.Graph.Algorithm.BreadthFirstSearch
  ( bfs
  ) where

import Data.Sequence as S
import Data.Monoid

import Data.Graph.Class
import Data.Graph.Class.AdjacencyList
import Data.Graph.Algorithm.GraphSearch

-- | Breadth first search visitor
newtype Queue v = Queue {runQueue :: Seq v}

instance Monoid (Queue v) where
  mempty = Queue mempty
  mappend (Queue q) (Queue q') = Queue (mappend q q')

instance Container (Queue v) where
  type Elem (Queue v) = v
  emptyC = Queue empty
  nullC (Queue q) = S.null q
  getC (viewl . runQueue -> (a :< q)) = (a, Queue q)
  getC _ = error "Queue is empty"
  putC v (Queue q) = Queue (q |> v)
  concatC (Queue q) (Queue q') = Queue (q >< q')

bfs :: (AdjacencyListGraph g, Monoid m) => Queue (Vertex g) -> GraphSearch g m -> Vertex g -> g m
bfs q vis v0 = graphSearch q vis v0
