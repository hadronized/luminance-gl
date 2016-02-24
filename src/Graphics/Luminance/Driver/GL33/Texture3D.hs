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

module Graphics.Luminance.Driver.GL33.Texture3D where

import Data.Foldable ( for_ )
import Data.Proxy ( Proxy(..) )
import Data.Vector.Storable ( unsafeWith )
import Foreign.Ptr ( castPtr, nullPtr )
import Graphics.Luminance.Driver.GL33.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Driver.GL33.Pixel ( Pixel(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

-- |A 3D texture.
data Texture3D f = Texture3D {
    texture3DBase :: BaseTexture
  , texture3DW    :: Natural
  , texture3DH    :: Natural
  , texture3DD    :: Natural
  } deriving (Eq,Show)

instance (Pixel f) => Texture (Texture3D f) where
  type TextureSize (Texture3D f) = (Natural,Natural,Natural)
  type TextureOffset (Texture3D f) = (Natural,Natural,Natural)
  fromBaseTexture bt (w,h,d) = Texture3D bt w h d
  toBaseTexture = texture3DBase
  textureTypeEnum _ = GL_TEXTURE_3D
  textureSize (Texture3D _ w h d) = (w,h,d)
  textureStorage _ _ levels (w,h,d) = do
      for_ [0..levels-1] $ \lvl -> do
        let divisor = 2 ^ lvl
        glTexImage3D GL_TEXTURE_3D lvl (fromIntegral $ pixelIFormat pf)
          (fromIntegral w `div` divisor) (fromIntegral h `div` divisor)
          (fromIntegral d `div` divisor) 0 (pixelFormat pf) (pixelType pf) nullPtr
    where pf = Proxy :: Proxy f
  transferTexelsSub _ tid (x,y,z) (w,h,d) texels = do
      glBindTexture GL_TEXTURE_3D tid
      unsafeWith texels $ glTexSubImage3D GL_TEXTURE_3D 0 (fromIntegral x) (fromIntegral y)
        (fromIntegral z) (fromIntegral w) (fromIntegral h) (fromIntegral d) fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
  fillTextureSub p tid o w filling = transferTexelsSub p tid o w filling
