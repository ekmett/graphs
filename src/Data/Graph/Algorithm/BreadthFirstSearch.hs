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
  ( bfs, Bfs(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Monoid
import Data.Sequence

import Data.Graph.Class
import Data.Graph.Class.AdjacencyList
import Data.Graph.PropertyMap
import Data.Graph.Internal.Color
import Data.Graph.Algorithm.GraphSearch

-- | Breadth first search visitor
bfs :: (AdjacencyListGraph g, Monoid m) => Bfs g m -> Vertex g -> g m
bfs vis v0 = do
  m <- vertexMap White
  evalStateT (enqueue vis v0 >>= pump) (mempty, m)
  where
  pump lhs = dequeue (return lhs) $ \ v -> do
    adjs <- lift $ outEdges v
    children <- foldrM
      (\e m -> do
        v' <- target e
        color <- getS v'
        liftM (`mappend` m) $ case color of
          White -> (liftM2 mappend) (lift $ enterEdge vis e) (enqueue vis v')
          Grey -> lift $ grayTarget vis e
          Black -> lift $ blackTarget vis e
      ) mempty adjs
    putS v Black
    rhs <- lift $ exitVertex vis v
    pump $ lhs `mappend` children `mappend` rhs
