{-# LANGUAGE CPP, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Algorithm.BreadthFirstSearch
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
-- Breadth-first search
----------------------------------------------------------------------------

module Data.Graph.Algorithm.BreadthFirstSearch
  ( bfs
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Sequence (Seq(..), ViewL(..), (|>), viewl)

import Data.Graph.Algorithm
import Data.Graph.Class
import Data.Graph.Class.AdjacencyList
import Data.Graph.PropertyMap
import Data.Graph.Internal.Color

getS :: Monad g => k -> StateT (Seq v, PropertyMap g k Color) g Color
getS k = do
  m <- gets snd
  lift (getP m k)

putS :: Monad g => k -> Color -> StateT (Seq v, PropertyMap g k Color) g ()
putS k v = do
  m <- gets snd
  m' <- lift $ putP m k v
  modify $ \(q,_) -> (q, m')

enqueue :: Graph g
        => GraphSearch g m
        -> Vertex g
        -> StateT (Seq (Vertex g), PropertyMap g (Vertex g) Color) g m
enqueue vis v = do
  m <- gets snd
  m' <- lift $ putP m v Grey
  modify $ \(q,_) -> (q |> v, m')
  lift $ enterVertex vis v

dequeue :: Monad g => StateT (Seq v, s) g r -> (v -> StateT (Seq v, s) g r) -> StateT (Seq v, s) g r
dequeue ke ks = do
  (q, m) <- get
  case viewl q of
    EmptyL -> ke
    (a :< q') -> put (q', m) >> ks a

bfs :: (AdjacencyListGraph g, Monoid m) => GraphSearch g m -> Vertex g -> g m
bfs vis v0 = do
  m <- vertexMap White
  evalStateT (enqueue vis v0 >>= pump) (mempty, m)
  where
  pump lhs = dequeue (return lhs) $ \ v -> do
    adjs <- lift $ outEdges v
    children <- foldrM
      (\e m -> do
        v' <- target e
        color <- getS v'
        liftM (`mappend` m) $ case color of
          White -> (liftM2 mappend) (lift $ enterEdge vis e) (enqueue vis v')
          Grey -> lift $ grayTarget vis e
          Black -> lift $ blackTarget vis e
      ) mempty adjs
    putS v Black
    rhs <- lift $ exitVertex vis v
    pump $ lhs `mappend` children `mappend` rhs
