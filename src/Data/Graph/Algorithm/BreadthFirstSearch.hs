{-# LANGUAGE TypeFamilies #-}
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

import Data.Monoid
import Data.Sequence

import Data.Graph.Class
import Data.Graph.Class.AdjacencyList
import Data.Graph.Algorithm.GraphSearch

-- | Breadth first search visitor
newtype Queue v = Queue (Seq v)

instance Monoid (Queue v) where
  mempty = Queue mempty
  mappend (Queue q) (Queue q') = Queue (mappend q q')

instance Container (Queue v) where
  type Elem (Queue v) = v

  getC (Queue q)   = case viewl q of
                       (a :< q') -> Just (a, Queue q')
                       _         -> Nothing
  putC v (Queue q) = Queue (q |> v)

bfs :: (AdjacencyListGraph g, Monoid m) => GraphSearch g m -> Vertex g -> g m
bfs vis v0 = bfs' mempty vis v0
  where
    bfs' :: (AdjacencyListGraph g, Monoid m) => Queue (Vertex g) -> GraphSearch g m -> Vertex g -> g m
    bfs' q vis' v0' = graphSearch q vis' v0'
