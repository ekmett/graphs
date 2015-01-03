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
newtype Bfs g m = Bfs (GraphSearch g m)

-- class Graph g => Collection g where
--   data Container g v :: *
--   emptyC  :: Container g v
--   nullC   :: Container g v -> Bool
--   getC    :: Container g v -> (v, Container g v)
--   putC    :: v -> Container g v -> Container g v
--   concatC :: Container g v -> Container g v -> Container g v
