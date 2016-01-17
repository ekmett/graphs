{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances #-}
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
  ( AdjacencyMatrixGraph(..)
  , module Data.Graph.Class
  ) where

import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class
import Data.Functor.Identity
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Graph.Class

class Graph g => AdjacencyMatrixGraph g where
  edge :: Vertex g -> Vertex g -> g (Maybe (Edge g))

instance AdjacencyMatrixGraph g => AdjacencyMatrixGraph (Strict.StateT s g) where
  edge a b = lift (edge a b)

instance AdjacencyMatrixGraph g => AdjacencyMatrixGraph (Lazy.StateT s g) where
  edge a b = lift (edge a b)

instance (AdjacencyMatrixGraph g, Monoid m) => AdjacencyMatrixGraph (Strict.WriterT m g) where
  edge a b = lift (edge a b)

instance (AdjacencyMatrixGraph g, Monoid m) => AdjacencyMatrixGraph (Lazy.WriterT m g) where
  edge a b = lift (edge a b)

instance (AdjacencyMatrixGraph g, Monoid m) => AdjacencyMatrixGraph (Strict.RWST r m s g) where
  edge a b = lift (edge a b)

instance (AdjacencyMatrixGraph g, Monoid m) => AdjacencyMatrixGraph (Lazy.RWST r m s g) where
  edge a b = lift (edge a b)

instance AdjacencyMatrixGraph g => AdjacencyMatrixGraph (MaybeT g) where
  edge a b = lift (edge a b)

instance (AdjacencyMatrixGraph g, Error e) => AdjacencyMatrixGraph (ErrorT e g) where
  edge a b = lift (edge a b)

instance AdjacencyMatrixGraph g => AdjacencyMatrixGraph (IdentityT g) where
  edge a b = lift (edge a b)

instance AdjacencyMatrixGraph g => AdjacencyMatrixGraph (ReaderT e g) where
  edge a b = lift (edge a b)

instance AdjacencyMatrixGraph Identity where
  edge _ _ = Identity Nothing
