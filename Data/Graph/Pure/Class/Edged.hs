{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Pure.Class.Edged
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Pure.Class.Edged 
  ( EdgedGraph(..)
  , module Data.Graph.Pure.Class
  ) where

import Data.Graph.Pure.Class

class (Graph g, Eq (Edge g)) => EdgedGraph g where
  type Edge g
