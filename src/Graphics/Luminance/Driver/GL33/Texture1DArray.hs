{-# LANGUAGE DataKinds #-}
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

module Graphics.Luminance.Driver.GL33.Texture1DArray where

import Data.Foldable ( for_ )
import Data.Proxy ( Proxy(..) )
import Data.Vector.Storable ( unsafeWith )
import Foreign.Ptr ( castPtr, nullPtr )
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Graphics.Luminance.Driver.GL33.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Driver.GL33.Pixel ( Pixel(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

-- |A 1D texture array.
data Texture1DArray (n :: Nat) (f :: *) = Texture1DArray {
    texture1DArrayBase :: BaseTexture
  , texture1DArrayW    :: Natural
  } deriving (Eq,Show)

instance (KnownNat n,Pixel f) => Texture (Texture1DArray n f) where
  -- |(w)
  type TextureSize (Texture1DArray n f) = Natural
  -- |(layer,x)
  type TextureOffset (Texture1DArray n f) = (Natural,Natural)
  fromBaseTexture = Texture1DArray
  toBaseTexture = texture1DArrayBase
  textureTypeEnum _ = GL_TEXTURE_1D_ARRAY
  textureSize = texture1DArrayW
  textureStorage _ _ levels w = do
      for_ [0..levels-1] $ \lvl -> do
        let divisor = 2 ^ lvl
        glTexImage2D GL_TEXTURE_1D_ARRAY lvl (fromIntegral $ pixelIFormat pf)
          (fromIntegral w `div` divisor) (fromIntegral $ natVal (Proxy :: Proxy n)) 0
          (pixelFormat pf) (pixelType pf) nullPtr
    where pf = Proxy :: Proxy f
  transferTexelsSub _ tid (layer,x) w texels = do
      glBindTexture GL_TEXTURE_1D_ARRAY tid
      unsafeWith texels $ glTexSubImage2D GL_TEXTURE_1D_ARRAY 0 (fromIntegral x) (fromIntegral w) (fromIntegral layer) 1 fmt
        typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
  fillTextureSub p tid o w filling = transferTexelsSub p tid o w filling
