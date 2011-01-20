{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Algorithm.DepthFirstSearch
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MTPCs, fundeps, type families
--
-- Depth-first search
----------------------------------------------------------------------------

module Data.Graph.Algorithm.DepthFirstSearch
  ( dfs, Dfs(..)
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

data Color = White | Gray | Black deriving (Eq,Ord,Show,Read)

data Dfs g m = Dfs 
  { enterVertex   :: Vertex g -> m             -- called the first time a vertex is discovered
  , backEdge      :: Vertex g -> Vertex g -> m -- called when we encounter a back edge to a vertex we're still processing
  , exitVertex    :: Vertex g -> m             -- called once we have processed all descendants of a vertex
  , crossEdge     :: Vertex g -> Vertex g -> m -- called when we encounter a cross edge to a vertex we've already finished
  }

instance Monoid m => Default (Dfs g m) where
  def = Dfs 
    (const mempty)
    (const mempty)
    (const mempty)
    (const mempty)

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
    putS v Gray
    adjs <- lift $ adjacentVertices v 
    result <- foldrM 
      (\v' m -> do 
        color <- getS v'
        liftM (`mappend` m) $ case color of
          White -> go v'
          Gray  -> return $ backEdge vis v v'
          Black -> return $ crossEdge vis v v'
      ) 
      mempty 
      adjs
    putS v Black
    return $ enterVertex vis v `mappend` result `mappend` exitVertex vis v
