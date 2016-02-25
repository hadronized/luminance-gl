{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Base ( MonadBase )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Either ( EitherT )
import Control.Monad.Trans.Resource ( MonadResource(..), MonadThrow, ResourceT )
import qualified Graphics.Luminance.Driver.GL33.Buffer as GL33
import Graphics.Luminance.BufferDriver ( BufferDriver(..) )

-- |OpenGL 3.3 luminance interpreter.
newtype GL33 a = GL33 { runGL33 :: EitherT GL33Error (ResourceT IO) a } deriving (Applicative,Functor,Monad,MonadIO,MonadBase IO,MonadThrow)

instance MonadResource GL33 where
  liftResourceT = GL33 . lift

instance BufferDriver GL33 where
  type BuildBuffer GL33 = GL33.BuildBuffer
  type Buffer GL33 = GL33.Buffer
  type BufferRW GL33 = GL33.BufferRW
  createRegion = GL33.createRegion
  createBuffer = GL33.createBuffer
  readWhole = GL33.readWhole
  writeWhole = GL33.writeWhole
  fill = GL33.fill
  (@?) = (GL33.@?)
  (@!) = (GL33.@!)
  writeAt = GL33.writeAt
  writeAt' = GL33.writeAt'

-- |OpenGL 3.3 error type.
data GL33Error
