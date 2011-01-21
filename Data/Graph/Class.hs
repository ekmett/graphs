{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Class 
  ( Graph(..)
  ) where

import Control.Monad
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Graph.PropertyMap

class (Monad g, Eq (Vertex g), Eq (Edge g)) => Graph g where
  type Vertex g :: *
  type Edge g :: * 
  vertexMap :: a -> g (PropertyMap g (Vertex g) a)
  edgeMap   :: a -> g (PropertyMap g (Edge g) a)

instance Graph g => Graph (Strict.StateT s g) where
  type Vertex (Strict.StateT s g) = Vertex g
  type Edge (Strict.StateT s g) = Edge g
  vertexMap = lift . liftM liftPropertyMap . vertexMap
  edgeMap = lift . liftM liftPropertyMap . edgeMap

instance Graph g => Graph (Lazy.StateT s g) where
  type Vertex (Lazy.StateT s g) = Vertex g
  type Edge (Lazy.StateT s g) = Edge g
  vertexMap = lift . liftM liftPropertyMap . vertexMap
  edgeMap = lift . liftM liftPropertyMap . edgeMap

instance (Graph g, Monoid m) => Graph (Strict.WriterT m g) where
  type Vertex (Strict.WriterT m g) = Vertex g
  type Edge (Strict.WriterT m g) = Edge g
  vertexMap = lift . liftM liftPropertyMap . vertexMap
  edgeMap = lift . liftM liftPropertyMap . edgeMap

instance (Graph g, Monoid m) => Graph (Lazy.WriterT m g) where
  type Vertex (Lazy.WriterT m g) = Vertex g
  type Edge (Lazy.WriterT m g) = Edge g
  vertexMap = lift . liftM liftPropertyMap . vertexMap
  edgeMap = lift . liftM liftPropertyMap . edgeMap
