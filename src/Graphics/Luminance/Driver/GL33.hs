{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Driver.GL33 (
    -- *
  ) where

import qualified Graphics.Luminance.Driver.GL33.Buffer as GL33
import Graphics.Luminance.BufferDriver ( BufferDriver )

-- |OpenGL 3.3 luminance interpreter.
newtype GL33 m a = GL33 { runGL33 :: m a } deriving (Applicative,Functor,Monad)
