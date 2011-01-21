{-# LANGUAGE TypeFamilies #-}
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
  ( bfs, Bfs(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Foldable
import Data.Monoid
import Data.Sequence

import Data.Graph.Class
import Data.Graph.Class.AdjacencyList
import Data.Graph.PropertyMap
import Data.Graph.Internal.Color

-- | Breadth first search visitor 
data Bfs g m = Bfs 
  { enterVertex :: Vertex g -> g m -- called the first time a vertex is discovered
  , grayTarget  :: Edge g   -> g m -- called when we encounter a back edge to a vertex we're still processing
  , exitVertex  :: Vertex g -> g m -- called once we have processed all descendants of a vertex
  , blackTarget :: Edge g   -> g m -- called when we encounter a cross edge to a vertex we've already finished
  } 

instance Graph g => Functor (Bfs g) where
  fmap f (Bfs a b c d) = Bfs 
    (liftM f . a)
    (liftM f . b)
    (liftM f . c)
    (liftM f . d)

instance Graph g => Applicative (Bfs g) where
  pure a = Bfs 
    (const (return a))
    (const (return a))
    (const (return a))
    (const (return a))

  m <*> n = Bfs
    (\v -> enterVertex m v `ap` enterVertex n v)
    (\e -> grayTarget m e `ap`  grayTarget n e)
    (\v -> exitVertex m v `ap`  exitVertex n v)
    (\e -> blackTarget m e `ap` blackTarget n e)

instance Graph g => Monad (Bfs g) where
  return = pure
  m >>= f = Bfs
    (\v -> enterVertex m v >>= ($ v) . enterVertex . f)
    (\e -> grayTarget m e >>= ($ e) . grayTarget . f)
    (\v -> exitVertex m v >>= ($ v) . exitVertex . f)
    (\e -> blackTarget m e >>= ($ e) . blackTarget . f)

instance (Graph g, Monoid m) => Default (Bfs g m) where
  def = return mempty

instance (Graph g, Monoid m) => Monoid (Bfs g m) where
  mempty = return mempty
  mappend = liftM2 mappend

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
        => Bfs g m 
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

bfs :: (AdjacencyListGraph g, Monoid m) => Bfs g m -> Vertex g -> g m
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
          White -> enqueue vis v' 
          Grey -> lift $ grayTarget vis e
          Black -> lift $ blackTarget vis e
      ) mempty adjs
    putS v Black
    rhs <- lift $ exitVertex vis v 
    pump $ lhs `mappend` children `mappend` rhs
