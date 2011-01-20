{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs, fundeps
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

class (Monad g, Eq v, Eq e) => Graph g v e | g -> v e where
  vertexMap :: a -> g (PropertyMap g v a)
  edgeMap   :: a -> g (PropertyMap g e a)

instance Graph g v e => Graph (Strict.StateT s g) v e where
  vertexMap = lift . liftM liftPropertyMap . vertexMap
  edgeMap = lift . liftM liftPropertyMap . edgeMap

instance Graph g v e => Graph (Lazy.StateT s g) v e where
  vertexMap = lift . liftM liftPropertyMap . vertexMap
  edgeMap = lift . liftM liftPropertyMap . edgeMap

instance (Graph g v e, Monoid m) => Graph (Strict.WriterT m g) v e where
  vertexMap = lift . liftM liftPropertyMap . vertexMap
  edgeMap = lift . liftM liftPropertyMap . edgeMap

instance (Graph g v e, Monoid m) => Graph (Lazy.WriterT m g) v e where
  vertexMap = lift . liftM liftPropertyMap . vertexMap
  edgeMap = lift . liftM liftPropertyMap . edgeMap
