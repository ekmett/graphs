{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.Edged
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Class.Edged 
  ( EdgedGraph(..)
  , module Data.Graph.Class
  ) where

import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Data.Monoid
import Data.Graph.Class

class (Graph g, Eq (Edge g)) => EdgedGraph g where
  type Edge g

instance EdgedGraph g => EdgedGraph (Strict.StateT s g) where
  type Edge (Strict.StateT s g) = Edge g

instance EdgedGraph g => EdgedGraph (Lazy.StateT s g) where
  type Edge (Lazy.StateT s g) = Edge g

instance (EdgedGraph g, Monoid m) => EdgedGraph (Strict.WriterT m g) where
  type Edge (Strict.WriterT m g) = Edge g

instance (EdgedGraph g, Monoid m) => EdgedGraph (Lazy.WriterT m g) where
  type Edge (Lazy.WriterT m g) = Edge g
