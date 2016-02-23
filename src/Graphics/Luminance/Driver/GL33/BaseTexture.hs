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

module Graphics.Luminance.Driver.GL33.BaseTexture where

import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.Proxy ( Proxy(..) )
import Data.Vector.Storable ( Vector )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( Storable(peek) )
import Graphics.GL
import Graphics.Luminance.Driver.GL33.Debug
import Graphics.Luminance.Texture ( CompareFunc(..), Filter(..), Sampling(..), Wrap(..) )
import Numeric.Natural ( Natural )

fromWrap :: (Eq a,Num a) => Wrap -> a
fromWrap w = case w of
  ClampToEdge    -> GL_CLAMP_TO_EDGE
  Repeat         -> GL_REPEAT
  MirroredRepeat -> GL_MIRRORED_REPEAT

fromFilter :: (Eq a,Num a) => Filter -> a
fromFilter f = case f of
  Nearest -> GL_NEAREST
  Linear  -> GL_LINEAR

fromCompareFunc :: (Eq a,Num a) => CompareFunc -> a
fromCompareFunc f = case f of
  Never          -> GL_NEVER
  Less           -> GL_LESS
  Equal          -> GL_EQUAL
  LessOrEqual    -> GL_LEQUAL
  Greater        -> GL_GREATER
  GreaterOrEqual -> GL_GEQUAL
  NotEqual       -> GL_NOTEQUAL
  Always         -> GL_ALWAYS

newtype BaseTexture = BaseTexture { baseTextureID  :: GLuint } deriving (Eq,Show)

class Texture t where
  type TextureSize t :: *
  type TextureOffset t :: *
  fromBaseTexture :: BaseTexture -> t

createTexture :: forall m t. (MonadIO m,MonadResource m,Texture t)
              => TextureSize t
              -> Natural
              -> Sampling
              -> m t
createTexture size levels sampling = do
    tid <- liftIO . alloca $ \p -> do
      debugGL $ glGenTextures 1 p
      tid <- peek p
      debugGL $ glBindTexture target tid
      debugGL $ glTexParameteri target GL_TEXTURE_BASE_LEVEL 0
      debugGL $ glTexParameteri target GL_TEXTURE_MAX_LEVEL (fromIntegral levels - 1)
      setTextureSampling target sampling
      textureStorage (Proxy :: Proxy t) tid (fromIntegral levels) size
      pure tid
    _ <- register $ with tid (glDeleteTextures 1)
    pure $ fromBaseTexture (BaseTexture tid) size
  where
    target = textureTypeEnum (Proxy :: Proxy t)

-- Apply a 'Sampling' object for a given type of object (texture, sampler, etc.).
setSampling :: (Eq a,Eq b,MonadIO m,Num a,Num b) => (GLenum -> a -> b -> IO ()) -> GLenum -> Sampling -> m ()
setSampling f oid s = liftIO $ do
  -- wraps
  debugGL $ f oid GL_TEXTURE_WRAP_S . fromWrap $ samplingWrapS s
  debugGL $ f oid GL_TEXTURE_WRAP_T . fromWrap $ samplingWrapT s
  debugGL $ f oid GL_TEXTURE_WRAP_R . fromWrap $ samplingWrapR s
  -- filters
  debugGL $ f oid GL_TEXTURE_MIN_FILTER . fromFilter $ samplingMinFilter s
  debugGL $ f oid GL_TEXTURE_MAG_FILTER . fromFilter $ samplingMagFilter s
  -- comparison function
  case samplingCompareFunction s of
    Just cmpf -> do
      debugGL $ f oid GL_TEXTURE_COMPARE_FUNC $ fromCompareFunc cmpf
      debugGL $ f oid GL_TEXTURE_COMPARE_MODE GL_COMPARE_REF_TO_TEXTURE
    Nothing ->
      debugGL $ f oid GL_TEXTURE_COMPARE_MODE GL_NONE

setTextureSampling :: (MonadIO m) => GLenum -> Sampling -> m ()
setTextureSampling = setSampling glTexParameteri

setSamplerSampling :: (MonadIO m) => GLenum -> Sampling -> m ()
setSamplerSampling = setSampling glSamplerParameteri

----------------------------------------------------------------------------------------------------
-- Texture operations ------------------------------------------------------------------------------

uploadSub :: forall a m t. (MonadIO m,Storable a,Texture t)
          => t
          -> TextureOffset t
          -> TextureSize t
          -> Bool
          -> Vector a
          -> m ()
uploadSub tex offset size autolvl texels = liftIO $ do
    transferTexelsSub (Proxy :: Proxy t) tid offset size texels
    debugGL . when autolvl $ glGenerateMipmap (textureTypeEnum (Proxy :: Proxy t))
  where
    tid = baseTextureID (toBaseTexture tex)

-- |Fill a subpart of the texture’s storage with a given value.
fillSub :: forall a m t. (MonadIO m,Storable a,Texture t)
        => t
        -> TextureOffset t
        -> TextureSize t
        -> Bool
        -> Vector a
        -> m ()
fillSub tex offset size autolvl filling = liftIO $ do
    fillTextureSub (Proxy :: Proxy t) tid offset size filling
    debugGL . when autolvl $ glGenerateMipmap (textureTypeEnum (Proxy :: Proxy t))
  where
    tid = baseTextureID (toBaseTexture tex)
