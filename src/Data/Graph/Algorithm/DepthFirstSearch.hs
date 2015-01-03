{-# LANGUAGE TypeFamilies, ViewPatterns #-}
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

import Data.Sequence as S
import Data.Monoid

import Data.Graph.Class
import Data.Graph.Class.AdjacencyList
import Data.Graph.Algorithm.GraphSearch

-- | Depth first search visitor
newtype Stack v = Stack {runStack :: Seq v}

instance Monoid (Stack v) where
  mempty = Stack mempty
  mappend (Stack q) (Stack q') = Stack (mappend q q')

instance Container (Stack v) where
  type Elem (Stack v) = v
  emptyC = Stack empty
  nullC (Stack q) = S.null q
  getC (viewr . runStack -> (q :> a)) = (a, Stack q)
  getC _ = error "Stack is empty"
  putC v (Stack q) = Stack (q |> v)
  concatC (Stack q) (Stack q') = Stack (q >< q')

dfs :: (AdjacencyListGraph g, Monoid m) => Stack (Vertex g) -> GraphSearch g m -> Vertex g -> g m
dfs q vis v0 = graphSearch q vis v0
