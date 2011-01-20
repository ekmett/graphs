{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.AdjacencyMatrix
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Class.AdjacencyMatrix 
  ( AdjacencyMatrix(..)
  , module Data.Graph.Class.Edged
  ) where

import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Graph.Class.Edged
import qualified Data.Graph.Pure.Class.AdjacencyMatrix as Pure

class EdgedGraph g => AdjacencyMatrix g where
  edge :: Vertex g -> Vertex g -> g (Maybe (Edge g))

instance AdjacencyMatrix g => AdjacencyMatrix (Strict.StateT s g) where
  edge a b = lift (edge a b)

instance AdjacencyMatrix g => AdjacencyMatrix (Lazy.StateT s g) where
  edge a b = lift (edge a b)

instance (AdjacencyMatrix g, Monoid m) => AdjacencyMatrix (Strict.WriterT m g) where
  edge a b = lift (edge a b)

instance (AdjacencyMatrix g, Monoid m) => AdjacencyMatrix (Lazy.WriterT m g) where
  edge a b = lift (edge a b)

instance Pure.AdjacencyMatrix g => AdjacencyMatrix ((->) g) where
  edge = Pure.edge
