{-# LANGUAGE CPP, TypeFamilies, FlexibleContexts #-}
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
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class
import Data.Functor.Identity
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
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

instance (EdgeEnumerableGraph g, Monoid m) => EdgeEnumerableGraph (Strict.RWST r m s g) where
  edges = lift edges

instance (EdgeEnumerableGraph g, Monoid m) => EdgeEnumerableGraph (Lazy.RWST r m s g) where
  edges = lift edges

instance EdgeEnumerableGraph g => EdgeEnumerableGraph (MaybeT g) where
  edges = lift edges

instance EdgeEnumerableGraph g => EdgeEnumerableGraph (IdentityT g) where
  edges = lift edges

instance (EdgeEnumerableGraph g, Error e) => EdgeEnumerableGraph (ErrorT e g) where
  edges = lift edges

instance EdgeEnumerableGraph g => EdgeEnumerableGraph (ReaderT e g) where
  edges = lift edges

instance EdgeEnumerableGraph Identity where
  edges = Identity []
