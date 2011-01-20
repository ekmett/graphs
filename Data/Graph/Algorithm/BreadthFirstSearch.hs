{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Algorithm.BreadthFirstSearch
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MTPCs, fundeps, type families
--
-- Breadth-first search
----------------------------------------------------------------------------

module Data.Graph.Algorithm.BreadthFirstSearch
  ( bfs, Bfs(..)
  ) where

import Data.Default
import Data.Foldable
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Monoid
import Data.Graph.Class
import Data.Graph.Class.Adjacency.List
import Data.Graph.PropertyMap
import Data.Sequence

data Color = White | Gray | Black deriving (Eq,Ord,Show,Read)

data Bfs v e m = Bfs 
  { enterVertex :: v -> m -- called the first time a vertex is discovered
  , grayTarget  :: e -> m -- called when we encounter a back edge to a vertex we're still processing
  , exitVertex  :: v -> m -- called once we have processed all descendants of a vertex
  , blackTarget :: e -> m -- called when we encounter a cross edge to a vertex we've already finished
  } deriving (Functor)

instance Monoid m => Default (Bfs v e m) where
  def = Bfs 
    (const mempty)
    (const mempty)
    (const mempty)
    (const mempty)

getS :: Monad g => k -> StateT (Seq v, PropertyMap g k Color) g Color
getS k = do
  m <- gets snd 
  lift (getP m k)

putS :: Monad g => k -> Color -> StateT (Seq v, PropertyMap g k Color) g ()
putS k v = do
  m <- gets snd 
  m' <- lift $ putP m k v
  modify $ \(q,_) -> (q, m')

enqueue :: Monad g => Bfs v e m -> v -> StateT (Seq v, PropertyMap g v Color) g m
enqueue vis v = do
  m <- gets snd
  m' <- lift $ putP m v Gray
  modify $ \(q,_) -> (q |> v, m')
  return $ enterVertex vis v

dequeue :: Monad g => StateT (Seq v, s) g r -> (v -> StateT (Seq v, s) g r) -> StateT (Seq v, s) g r
dequeue ke ks = do
  (q, m) <- get
  case viewl q of
    EmptyL      -> ke
    (a :< q') -> put (q', m) >> ks a

-- TODO: CPS transform?
bfs :: (AdjacencyListGraph g v e, Monoid m) => Bfs v e m -> v -> g m
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
          Gray -> return $ grayTarget vis e
          Black -> return $ blackTarget vis e)
      mempty
      adjs
    putS v Black
    pump $ lhs `mappend` children `mappend` exitVertex vis v
