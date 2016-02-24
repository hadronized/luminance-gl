{-# LANGUAGE ScopedTypeVariables #-}
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

module Graphics.Luminance.Driver.GL33.Cubemap where

import Data.Proxy ( Proxy(..) )
import Data.Foldable ( for_ )
import Data.Vector.Storable ( unsafeWith )
import Foreign.Ptr ( castPtr )
import Foreign.Ptr ( nullPtr )
import Graphics.Luminance.Driver.GL33.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Driver.GL33.Pixel ( Pixel(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

-- |Face of a 'Cubemap'.
data CubeFace
  = PositiveX
  | NegativeX
  | PositiveY
  | NegativeY
  | PositiveZ
  | NegativeZ
    deriving (Eq,Show)

fromCubeFace :: CubeFace -> GLint
fromCubeFace f = case f of
  PositiveX -> GL_TEXTURE_CUBE_MAP_POSITIVE_X
  NegativeX -> GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  PositiveY -> GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  NegativeY -> GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  PositiveZ -> GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  NegativeZ -> GL_TEXTURE_CUBE_MAP_NEGATIVE_Z

-- |A cubemap.
data Cubemap f = Cubemap {
    cubemapBase :: BaseTexture
  , cubemapSize :: Natural
  } deriving (Eq,Show)

instance (Pixel f) => Texture (Cubemap f) where
  type TextureSize (Cubemap f) = Natural
  type TextureOffset (Cubemap f) = (Natural,Natural,CubeFace)
  fromBaseTexture = Cubemap
  toBaseTexture = cubemapBase
  textureTypeEnum _ = GL_TEXTURE_CUBE_MAP
  textureSize (Cubemap _ s) = s
  textureStorage _ _ levels s = do
      for_ [0..levels-1] $ \lvl -> do
        let divisor = 2 ^ lvl
        glTexImage2D GL_TEXTURE_CUBE_MAP lvl (fromIntegral $ pixelIFormat pf)
          (fromIntegral s `div` divisor) (fromIntegral s `div` divisor) 0 (pixelFormat pf)
          (pixelType pf) nullPtr
    where pf = Proxy :: Proxy f
  transferTexelsSub _ tid (x,y,f) s texels = do
      glBindTexture GL_TEXTURE_CUBE_MAP tid
      unsafeWith texels $ glTexSubImage3D GL_TEXTURE_CUBE_MAP 0 (fromIntegral x) (fromIntegral y)
        (fromCubeFace f) (fromIntegral s) (fromIntegral s) 1 fmt
        typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
  fillTextureSub p tid o w filling = transferTexelsSub p tid o w filling
