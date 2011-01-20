{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Pure.Class
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Pure.Class 
  ( Graph(..)
  , VertexMap
  ) where

import Data.Graph.PropertyMap

type VertexMap g = PropertyMap ((->)g) (Vertex g) 

class Eq (Vertex g) => Graph g where
  type Vertex g
  vertexMap :: a -> g -> VertexMap g a
