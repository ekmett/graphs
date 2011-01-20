{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Pure.Class.AdjacencyMatrix
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Pure.Class.AdjacencyMatrix 
  ( AdjacencyMatrix(..)
  , module Data.Graph.Pure.Class.Edged
  ) where

import Data.Graph.Pure.Class.Edged

class EdgedGraph g => AdjacencyMatrix g where
  edge :: Vertex g -> Vertex g -> g -> Maybe (Edge g)
