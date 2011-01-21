{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.EdgeEnumerable
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Class.EdgeEnumerable
  ( EdgeEnumerableGraph(..)
  , module Data.Graph.Class
  ) where

import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Graph.Class

class Graph g => EdgeEnumerableGraph g where
  -- | /O(e)/
  edges :: g [Edge g]

instance EdgeEnumerableGraph g => EdgeEnumerableGraph (Strict.StateT s g) where
  edges = lift edges

instance EdgeEnumerableGraph g => EdgeEnumerableGraph (Lazy.StateT s g) where
  edges = lift edges

instance (EdgeEnumerableGraph g, Monoid m) => EdgeEnumerableGraph (Strict.WriterT m g) where
  edges = lift edges

instance (EdgeEnumerableGraph g, Monoid m) => EdgeEnumerableGraph (Lazy.WriterT m g) where
  edges = lift edges
