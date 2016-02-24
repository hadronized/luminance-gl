{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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

module Graphics.Luminance.Driver.GL33.CubemapArray where

import GHC.TypeLits ( KnownNat, Nat )
import Graphics.Luminance.Driver.GL33.Cubemap ( CubeFace )
import Graphics.Luminance.Driver.GL33.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Driver.GL33.Pixel ( Pixel(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

-- |A cubemap array.
data CubemapArray (n :: Nat) (f :: *) = CubemapArray {
    cubemapArrayBase :: BaseTexture
  , cubemapArraySize :: Natural
  } deriving (Eq,Show)

instance (KnownNat n, Pixel f) => Texture (CubemapArray n f) where
  -- |(w,h)
  type TextureSize (CubemapArray n f) = Natural
  -- |(layer,x,y,face)
  type TextureOffset (CubemapArray n f) = (Natural,Natural,Natural,CubeFace)
  fromBaseTexture bt s = CubemapArray bt s
  toBaseTexture = cubemapArrayBase
  textureTypeEnum _ = GL_TEXTURE_CUBE_MAP_ARRAY
  textureSize (CubemapArray _ s) = s
  textureStorage _ _ _ _ = pure ()
  transferTexelsSub _ _ _ _ _ = pure ()
  fillTextureSub _ _ _ _ _ = pure ()
