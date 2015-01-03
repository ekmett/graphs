{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Algorithm.DepthFirstSearch
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
-- Depth-first search
----------------------------------------------------------------------------

module Data.Graph.Algorithm.DepthFirstSearch
  ( dfs
  ) where

import Data.Monoid

import Data.Graph.Class
import Data.Graph.Class.AdjacencyList
import Data.Graph.Algorithm.GraphSearch

-- | Depth first search visitor
newtype Stack v = Stack [v]

instance Monoid (Stack v) where
  mempty = Stack []
  mappend (Stack q) (Stack q') = Stack (q ++ q')

instance Container (Stack v) where
  type Elem (Stack v) = v

  getC (Stack [])     = Nothing
  getC (Stack (x:xs)) = Just (x, Stack xs)
  putC v (Stack q)    = Stack (v : q)

dfs :: (AdjacencyListGraph g, Monoid m) => Stack (Vertex g) -> GraphSearch g m -> Vertex g -> g m
dfs q vis v0 = graphSearch q vis v0
