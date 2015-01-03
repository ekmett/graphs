{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Algorithm.GraphSearch
-- Copyright   :  (C) 2011-2015 Edvard Kmett, Jeffrey Rosenbluth
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
-- Graph search
----------------------------------------------------------------------------

module Data.Graph.Algorithm.GraphSearch
  ( graphSearch, GraphSearch(..), Container(..)
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

-- | Graph search visitor
data GraphSearch g m = GraphSearch
  { enterVertex :: Vertex g -> g m -- called the first time a vertex is discovered
  , enterEdge   :: Edge g   -> g m -- called the first time an edge is discovered, before enter
  , grayTarget  :: Edge g   -> g m -- called when we encounter a back edge to a vertex we're still processing
  , exitVertex  :: Vertex g -> g m -- called once we have processed all descendants of a vertex
  , blackTarget :: Edge g   -> g m -- called when we encounter a cross edge to a vertex we've already finished
  }

instance Graph g => Functor (GraphSearch g) where
  fmap f (GraphSearch a b c d e) = GraphSearch
    (liftM f . a)
    (liftM f . b)
    (liftM f . c)
    (liftM f . d)
    (liftM f . e)

instance Graph g => Applicative (GraphSearch g) where
  pure a = GraphSearch
    (const (return a))
    (const (return a))
    (const (return a))
    (const (return a))
    (const (return a))

  m <*> n = GraphSearch
    (\v -> enterVertex m v `ap` enterVertex n v)
    (\e -> enterEdge m e `ap`   enterEdge n e)
    (\e -> grayTarget m e `ap`  grayTarget n e)
    (\v -> exitVertex m v `ap`  exitVertex n v)
    (\e -> blackTarget m e `ap` blackTarget n e)

instance Graph g => Monad (GraphSearch g) where
  return = pure
  m >>= f = GraphSearch
    (\v -> enterVertex m v >>= ($ v) . enterVertex . f)
    (\e -> enterEdge m e >>= ($ e) . enterEdge . f)
    (\e -> grayTarget m e >>= ($ e) . grayTarget . f)
    (\v -> exitVertex m v >>= ($ v) . exitVertex . f)
    (\e -> blackTarget m e >>= ($ e) . blackTarget . f)

instance (Graph g, Monoid m) => Monoid (GraphSearch g m) where
  mempty = return mempty
  mappend = liftM2 mappend

class Container c where
  type Elem c :: *
  emptyC  :: c
  nullC   :: c -> Bool
  getC    :: c -> (Elem c, c)
  putC    :: Elem c -> c -> c
  concatC :: c -> c -> c

getS :: Monad g => k -> StateT (c, PropertyMap g k Color) g Color
getS k = do
  m <- gets snd
  lift (getP m k)

putS :: Monad g => k -> Color -> StateT (c, PropertyMap g k Color) g ()
putS k v = do
  m <- gets snd
  m' <- lift $ putP m k v
  modify $ \(q,_) -> (q, m')

insert :: (Graph g, Container c, Elem c ~ Vertex g)
        => GraphSearch g m
        -> Vertex g
        -> StateT (c, PropertyMap g (Vertex g) Color) g m
insert vis v = do
  m <- gets snd
  m' <- lift $ putP m v Grey
  modify $ \(q,_) -> (putC v q, m')
  lift $ enterVertex vis v

remove :: (Monad g, Container c)
        => StateT (c, s) g r -> (Elem c -> StateT (c, s) g r) -> StateT (c, s) g r
remove ke ks = do
  (q, m) <- get
  if nullC q
     then ke
     else let (a, q') = getC q in put (q', m) >> ks a

graphSearch :: forall g m c.
              (AdjacencyListGraph g, Monoid m, Container c, Monoid c, Elem c ~ Vertex g)
            => c -> GraphSearch g m -> Vertex g -> g m
graphSearch _ vis v0 = do
  m <- vertexMap White
  evalStateT (insert vis v0 >>= pump) (mempty, m)
  where
  pump :: m -> StateT (c, PropertyMap g (Vertex g) Color) g m
  pump lhs = remove (return lhs) $ \v -> do
    adjs <- lift $ outEdges v
    children <- foldrM
      (\e m -> do
        v' <- target e
        color <- getS v'
        liftM (`mappend` m) $ case color of
          White -> (liftM2 mappend) (lift $ enterEdge vis e) (insert vis v')
          Grey -> lift $ grayTarget vis e
          Black -> lift $ blackTarget vis e
      ) mempty adjs
    putS v Black
    rhs <- lift $ exitVertex vis v
    pump $ lhs `mappend` children `mappend` rhs
