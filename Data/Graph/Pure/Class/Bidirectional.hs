{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Pure.Class.Bidirectional
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Pure.Class.Bidirectional 
  ( BidirectionalGraph(..)
  , module Data.Graph.Pure.Class.Incidence
  ) where

import Control.Monad
import Data.Graph.Pure.Class.Incidence

class IncidenceGraph g => BidirectionalGraph g where
  -- /O(n)/
  inEdges :: Vertex g -> g -> [Edge g]
  -- /O(n)/
  inDegree :: Vertex g -> g -> Int
  inDegree v = length `liftM` inEdges v
  
  incidentEdges :: Vertex g -> g -> [Edge g]
  incidentEdges v = liftM2 (++) (inEdges v) (outEdges v)

  degree :: Vertex g -> g -> Int
  degree v = liftM2 (+) (inDegree v) (outDegree v)

