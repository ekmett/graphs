{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Algorithm.DepthFirstSearch
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
-- Depth-first search
----------------------------------------------------------------------------

module Data.Graph.Algorithm.DepthFirstSearch
  ( dfs, Dfs(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Monoid

import Data.Graph.Class
import Data.Graph.Class.AdjacencyList
import Data.Graph.PropertyMap
import Data.Graph.Internal.Color

data Dfs g m = Dfs 
  { enterVertex :: Vertex g -> g m -- called the first time a vertex is discovered
  , grayTarget  :: Edge g   -> g m -- called when we encounter a back edge to a vertex we're still processing
  , exitVertex  :: Vertex g -> g m -- called once we have processed all descendants of a vertex
  , blackTarget :: Edge g   -> g m -- called when we encounter a cross edge to a vertex we've already finished
  }

instance Graph g => Functor (Dfs g) where
  fmap f (Dfs a b c d) = Dfs 
    (liftM f . a)
    (liftM f . b)
    (liftM f . c)
    (liftM f . d)

instance Graph g => Applicative (Dfs g) where
  pure a = Dfs 
    (const (return a))
    (const (return a))
    (const (return a))
    (const (return a))

  m <*> n = Dfs
    (\v -> enterVertex m v `ap` enterVertex n v)
    (\e -> grayTarget m e `ap`  grayTarget n e)
    (\v -> exitVertex m v `ap`  exitVertex n v)
    (\e -> blackTarget m e `ap` blackTarget n e)

instance Graph g => Monad (Dfs g) where
  return = pure
  m >>= f = Dfs
    (\v -> enterVertex m v >>= ($ v) . enterVertex . f)
    (\e -> grayTarget m e >>= ($ e) . grayTarget . f)
    (\v -> exitVertex m v >>= ($ v) . exitVertex . f)
    (\e -> blackTarget m e >>= ($ e) . blackTarget . f)

instance (Graph g, Monoid m) => Monoid (Dfs g m) where
  mempty = return mempty
  mappend = liftM2 mappend

getS :: Monad g => k -> StateT (PropertyMap g k v) g v
getS k = do
  m <- get 
  lift (getP m k)

putS :: Monad g => k -> v -> StateT (PropertyMap g k v) g ()
putS k v = do
  m <- get 
  m' <- lift $ putP m k v
  put m'

-- TODO: CPS transform?
dfs :: (AdjacencyListGraph g, Monoid m) => Dfs g m -> Vertex g -> g m
dfs vis v0 = do
  m <- vertexMap White 
  evalStateT (go v0) m where
  go v = do
    putS v Grey
    lhs <- lift $ enterVertex vis v
    adjs <- lift $ outEdges v 
    result <- foldrM 
      (\e m -> do 
        v' <- target e
        color <- getS v'
        liftM (`mappend` m) $ case color of
          White -> go v'
          Grey  -> lift $ grayTarget vis e
          Black -> lift $ blackTarget vis e
      ) 
      mempty 
      adjs
    putS v Black
    rhs <- lift $ exitVertex vis v
    return $ lhs `mappend` result `mappend` rhs
