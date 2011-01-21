{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.VertexEnumerable
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Class.VertexEnumerable
  ( VertexEnumerableGraph(..)
  , module Data.Graph.Class
  ) where

import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Graph.Class

class Graph g => VertexEnumerableGraph g where
  -- | /O(v)/
  vertices :: g [Vertex g]

instance VertexEnumerableGraph g => VertexEnumerableGraph (Strict.StateT s g) where
  vertices = lift vertices

instance VertexEnumerableGraph g => VertexEnumerableGraph (Lazy.StateT s g) where
  vertices = lift vertices

instance (VertexEnumerableGraph g, Monoid m) => VertexEnumerableGraph (Strict.WriterT m g) where
  vertices = lift vertices

instance (VertexEnumerableGraph g, Monoid m) => VertexEnumerableGraph (Lazy.WriterT m g) where
  vertices = lift vertices

