{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Driver.GL33.Pixel where

import Data.Word ( Word8 )
import Graphics.GL
import Graphics.Luminance.Pixel ( RGB8UI, RGBA8UI, RGB32F, RGBA32F, Depth32F )

class Pixel f where
  type PixelBase f :: *
  pixelFormat  :: p f -> GLenum
  pixelIFormat :: p f -> GLenum
  pixelType    :: p f -> GLenum

instance Pixel RGB8UI where
  type PixelBase RGB8UI = Word8
  pixelFormat  _ = GL_RGB_INTEGER
  pixelIFormat _ = GL_RGB8UI
  pixelType    _ = GL_UNSIGNED_BYTE

instance Pixel RGBA8UI where
  type PixelBase RGBA8UI = Word8
  pixelFormat  _ = GL_RGBA_INTEGER
  pixelIFormat _ = GL_RGBA8UI
  pixelType    _ = GL_UNSIGNED_BYTE

instance Pixel RGB32F where
  type PixelBase RGB32F = Float
  pixelFormat  _ = GL_RGB
  pixelIFormat _ = GL_RGB32F
  pixelType    _ = GL_FLOAT

instance Pixel RGBA32F where
  type PixelBase RGBA32F = Float
  pixelFormat  _ = GL_RGBA
  pixelIFormat _ = GL_RGBA32F
  pixelType    _ = GL_FLOAT

instance Pixel Depth32F where
  type PixelBase Depth32F = Float
  pixelFormat  _ = GL_DEPTH_COMPONENT
  pixelIFormat _ = GL_DEPTH_COMPONENT32F
  pixelType    _ = GL_FLOAT
