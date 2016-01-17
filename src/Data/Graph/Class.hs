{-# LANGUAGE CPP, TypeFamilies, FlexibleContexts #-}
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
  , VertexMap
  , EdgeMap
  , liftVertexMap
  , liftEdgeMap
  ) where

import Control.Monad
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Functor.Identity
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Graph.PropertyMap
import Data.Void

type VertexMap g = PropertyMap g (Vertex g)
type EdgeMap g = PropertyMap g (Edge g)

class (Monad g, Eq (Vertex g), Eq (Edge g)) => Graph g where
  type Vertex g :: *
  type Edge g :: *
  vertexMap :: a -> g (VertexMap g a)
  edgeMap   :: a -> g (EdgeMap g a)

liftVertexMap :: (MonadTrans t, Graph (t g), Graph g, Vertex (t g) ~ Vertex g)
              => a -> t g (VertexMap (t g) a)
liftVertexMap = lift . liftM liftPropertyMap . vertexMap
{-# INLINE liftVertexMap #-}

liftEdgeMap :: (MonadTrans t, Graph (t g), Graph g, Edge (t g) ~ Edge g)
            => a -> t g (EdgeMap (t g) a)
liftEdgeMap = lift . liftM liftPropertyMap . edgeMap
{-# INLINE liftEdgeMap #-}

instance Graph g => Graph (Strict.StateT s g) where
  type Vertex (Strict.StateT s g) = Vertex g
  type Edge (Strict.StateT s g) = Edge g
  vertexMap = liftVertexMap
  edgeMap = liftEdgeMap

instance Graph g => Graph (Lazy.StateT s g) where
  type Vertex (Lazy.StateT s g) = Vertex g
  type Edge (Lazy.StateT s g) = Edge g
  vertexMap = liftVertexMap
  edgeMap = liftEdgeMap

instance (Graph g, Monoid m) => Graph (Strict.WriterT m g) where
  type Vertex (Strict.WriterT m g) = Vertex g
  type Edge (Strict.WriterT m g) = Edge g
  vertexMap = liftVertexMap
  edgeMap = liftEdgeMap

instance (Graph g, Monoid m) => Graph (Lazy.WriterT m g) where
  type Vertex (Lazy.WriterT m g) = Vertex g
  type Edge (Lazy.WriterT m g) = Edge g
  vertexMap = liftVertexMap
  edgeMap = liftEdgeMap

instance Graph g => Graph (ReaderT m g) where
  type Vertex (ReaderT m g) = Vertex g
  type Edge (ReaderT m g) = Edge g
  vertexMap = liftVertexMap
  edgeMap = liftEdgeMap

instance Graph g => Graph (IdentityT g) where
  type Vertex (IdentityT g) = Vertex g
  type Edge (IdentityT g) = Edge g
  vertexMap = liftVertexMap
  edgeMap = liftEdgeMap

instance Graph g => Graph (MaybeT g) where
  type Vertex (MaybeT g) = Vertex g
  type Edge (MaybeT g) = Edge g
  vertexMap = liftVertexMap
  edgeMap = liftEdgeMap

instance (Graph g, Error e) => Graph (ErrorT e g) where
  type Vertex (ErrorT e g) = Vertex g
  type Edge (ErrorT e g) = Edge g
  vertexMap = liftVertexMap
  edgeMap = liftEdgeMap

instance (Graph g, Monoid w) => Graph (Lazy.RWST r w s g) where
  type Vertex (Lazy.RWST r w s g) = Vertex g
  type Edge (Lazy.RWST r w s g) = Edge g
  vertexMap = liftVertexMap
  edgeMap = liftEdgeMap

instance (Graph g, Monoid w) => Graph (Strict.RWST r w s g) where
  type Vertex (Strict.RWST r w s g) = Vertex g
  type Edge (Strict.RWST r w s g) = Edge g
  vertexMap = liftVertexMap
  edgeMap = liftEdgeMap

voidMap :: PropertyMap Identity Void a
voidMap = PropertyMap (Identity . absurd) $ \_ _ -> Identity voidMap

-- | The empty graph
instance Graph Identity where
  type Vertex Identity = Void
  type Edge Identity = Void
  vertexMap _ = Identity voidMap
  edgeMap   _ = Identity voidMap

