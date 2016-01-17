{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.Bidirectional
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Class.Bidirectional
  ( BidirectionalGraph(..)
  , module Data.Graph.Class.AdjacencyList
  ) where

import Control.Monad
import Control.Monad.Trans.Class
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
import Data.Functor.Identity
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Graph.Class.AdjacencyList

class AdjacencyListGraph g => BidirectionalGraph g where
  -- /O(e)/
  inEdges :: Vertex g -> g [Edge g]
  -- /O(e)/
  inDegree :: Vertex g -> g Int
  inDegree v = length `liftM` inEdges v

  incidentEdges :: Vertex g -> g [Edge g]
  incidentEdges v = liftM2 (++) (inEdges v) (outEdges v)

  degree :: Vertex g -> g Int
  degree v = liftM2 (+) (inDegree v) (outDegree v)

instance BidirectionalGraph g => BidirectionalGraph (Strict.StateT s g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance BidirectionalGraph g => BidirectionalGraph (Lazy.StateT s g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance (BidirectionalGraph g, Monoid m) => BidirectionalGraph (Strict.WriterT m g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance (BidirectionalGraph g, Monoid m) => BidirectionalGraph (Lazy.WriterT m g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance (BidirectionalGraph g, Monoid m) => BidirectionalGraph (Strict.RWST r m s g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance (BidirectionalGraph g, Monoid m) => BidirectionalGraph (Lazy.RWST r m s g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance BidirectionalGraph g => BidirectionalGraph (ReaderT e g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance BidirectionalGraph g => BidirectionalGraph (IdentityT g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance BidirectionalGraph g => BidirectionalGraph (MaybeT g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance (BidirectionalGraph g, Error e) => BidirectionalGraph (ErrorT e g) where
  inEdges  = lift . inEdges
  inDegree = lift . inDegree
  incidentEdges = lift . incidentEdges
  degree = lift . degree

instance BidirectionalGraph Identity where
  inEdges _ = Identity []
  inDegree _ = Identity 0
  incidentEdges _ = Identity []
  degree _  = Identity 0

