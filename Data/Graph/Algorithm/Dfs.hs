{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Algorithm.Dfs
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MTPCs, fundeps, type families
--
-- Depth-first search
----------------------------------------------------------------------------

module Data.Graph.Algorithm.Dfs 
  ( dfs, Dfs(..)
  ) where

import Data.Default
import Data.Foldable
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Monoid
import Data.Graph.Class
import Data.Graph.Class.Adjacency
import Data.Graph.PropertyMap

data Color = White | Gray | Black deriving (Eq,Ord,Show,Read)

newtype Dfs g m = Dfs { discoverVertex :: Vertex g -> m }

instance Monoid m => Default (Dfs g m) where
  def = Dfs (const mempty)

getS :: Monad g => k -> StateT (PropertyMap g k v) g v
getS k = do
  m <- get 
  lift (getP m k)

putS :: Monad g => k -> v -> StateT (PropertyMap g k v) g ()
putS k v = do
  m <- get 
  m' <- lift $ putP m k v
  put m'

dfs :: (AdjacencyGraph g, Monoid m) => Dfs g m -> Vertex g -> g m
dfs vis v0 = vertexMap White >>= evalStateT (go v0) where 
  go v = do
    putS v Gray
    adjs <- lift $ adjacentVertices v 
    result <- foldrM 
      (\v' m -> do 
        color <- getS v'
        if color == White
          then liftM (`mappend` m) (go v')
          else return m
      ) 
      mempty 
      adjs
    putS v Black
    return $ discoverVertex vis v `mappend` result
