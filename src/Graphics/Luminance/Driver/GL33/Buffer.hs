{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Driver.GL33.Buffer where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.RWS ( RWS, ask, get, evalRWS, execRWS, put )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.Bits ( (.|.) )
import Data.Foldable ( toList )
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word32 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( peekArray, pokeArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, castPtr, nullPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.GL
import Graphics.Luminance.RW

-- Create a new buffer and return its GPU address by mapping it to a @Ptr ()@.
mkBuffer :: (MonadIO m,MonadResource m)
         => GLbitfield
         -> Int
         -> m (GLuint,Ptr ())
mkBuffer flags size = do
  (bid,mapped) <- liftIO . alloca $ \p -> do
    glGenBuffers 1 p
    bid <- peek p
    mapped <- createStorage bid flags size
    pure (bid,mapped)
  _ <- register . with bid $ glDeleteBuffers 1
  pure (bid,mapped)

-- Create the required OpenGL storage for a 'Buffer'.
createStorage :: GLuint -> GLbitfield -> Int -> IO (Ptr ())
createStorage bid _ size = do
    glBindBuffer GL_ARRAY_BUFFER bid
    glBufferData GL_ARRAY_BUFFER bytes nullPtr GL_STREAM_DRAW
    pure nullPtr
  where
    bytes = fromIntegral size

-- Create a new buffer using regions through the 'BuildBuffer monadic type. The buffer is
-- returned as well as the computed 'a' value  in the 'BuildBuffer.
--
-- Typically, the user will wrap the 'Buffer' in the 'a' type.
mkBufferWithRegions :: (MonadIO m,MonadResource m)
                    => GLbitfield
                    -> BuildBuffer rw a
                    -> m (a,GLuint)
mkBufferWithRegions flags buildRegions = do
    (buffer,mapped) <- mkBuffer flags bytes
    pure (fst $ evalRWS built (buffer,mapped) 0,buffer)
  where
    built = runBuildBuffer buildRegions
    (bytes,_) = execRWS built (0,nullPtr) 0

-- |'Buffer's can have reads and writes. That typeclass makes implements all possible
-- cases.
class BufferRW rw where
  bufferFlagsFromRW :: proxy rw -> GLenum

instance BufferRW R where
  bufferFlagsFromRW _ = GL_MAP_READ_BIT

instance BufferRW RW where
  bufferFlagsFromRW _ = GL_MAP_READ_BIT .|. GL_MAP_WRITE_BIT

instance BufferRW W where
  bufferFlagsFromRW _ = GL_MAP_WRITE_BIT

-- |Create a new 'Buffer'. Through the 'BuildBuild type, you can yield new regions and embed them
-- in the type of your choice. The function returns that type.
createBuffer :: forall a m rw. (BufferRW rw,MonadIO m,MonadResource m)
             => BuildBuffer rw a
             -> m a
createBuffer = fmap fst . mkBufferWithRegions (bufferFlagsFromRW (Proxy :: Proxy rw) .|. persistentCoherentBits)

persistentCoherentBits :: GLbitfield
persistentCoherentBits = 0

-- |A 'Buffer' is a GPU typed memory area. It can be pictured as a GPU array.
data Buffer rw a = Buffer {
    bufferOffset :: Int -- offset in the memory of the buffer
  , bufferSize :: Int -- number of elements living in that region
  , bufferID :: GLuint -- buffer the region lays in
  } deriving (Eq,Show)

-- |Convenient type to build 'Buffer's.
newtype BuildBuffer rw a = BuildBuffer {
    runBuildBuffer :: RWS (GLuint,Ptr ()) () Int a
  } deriving (Applicative,Functor,Monad)

-- |Create a new 'Buffer' by providing the number of wished elements.
newRegion :: forall rw a. (Storable a) => Word32 -> BuildBuffer rw (Buffer rw a)
newRegion size = BuildBuffer $ do
    offset <- get
    put $ offset + fromIntegral size * sizeOf (undefined :: a)
    (buffer,_) <- ask
    pure $ Buffer {
        bufferOffset = offset
      , bufferSize = fromIntegral size
      , bufferID = buffer
      }

-- |Read a whole 'Buffer'.
readWhole :: (MonadIO m,Readable r,Storable a) => Buffer r a -> m [a]
readWhole r = liftIO $ do
  glBindBuffer GL_ARRAY_BUFFER (bufferID r)
  p <- glMapBufferRange GL_ARRAY_BUFFER (fromIntegral $ bufferOffset r) (fromIntegral $ bufferSize r) GL_MAP_READ_BIT
  a <- peekArray (bufferSize r) (castPtr p)
  _ <- glUnmapBuffer GL_ARRAY_BUFFER
  pure a

-- |Write the whole 'Buffer'. If values are missing, only the provided values will replace the
-- existing ones. If there are more values than the size of the 'Buffer', they are ignored.
writeWhole :: (Foldable f,MonadIO m,Storable a,Writable w)
           => Buffer w a
           -> f a
           -> m ()
writeWhole r values = liftIO $ do
  glBindBuffer GL_ARRAY_BUFFER (bufferID r)
  p <- glMapBufferRange GL_ARRAY_BUFFER (fromIntegral $ bufferOffset r) (fromIntegral $ bufferSize r) GL_MAP_WRITE_BIT
  pokeArray (castPtr p) . take (bufferSize r) $ toList values
  () <$ glUnmapBuffer GL_ARRAY_BUFFER

-- |Fill a 'Buffer' with a value.
fill :: (MonadIO m,Storable a,Writable w) => Buffer w a -> a -> m ()
fill r a = writeWhole r (replicate (bufferSize r) a)

-- |Index getter. Bounds checking is performed and returns 'Nothing' if out of bounds.
(@?) :: (MonadIO m,Storable a,Readable r) => Buffer r a -> Word32 -> m (Maybe a)
r @? i
  | i >= fromIntegral (bufferSize r) = pure Nothing
  | otherwise = fmap Just (r @! i)

-- |Index getter. Unsafe version of '(@?)'.
(@!) :: (MonadIO m,Storable a,Readable r) => Buffer r a -> Word32 -> m a
r @! i = liftIO $ do
  glBindBuffer GL_ARRAY_BUFFER (bufferID r)
  p <- glMapBufferRange GL_ARRAY_BUFFER (fromIntegral $ bufferOffset r) (fromIntegral $ bufferSize r) GL_MAP_READ_BIT
  a <- peekElemOff (castPtr p) (fromIntegral i)
  _ <- glUnmapBuffer GL_ARRAY_BUFFER
  pure a

-- |Index setter. Bounds checking is performed and nothing is done if out of bounds.
writeAt :: (MonadIO m,Storable a,Writable w) => Buffer w a -> Word32 -> a -> m ()
writeAt r i a
  | i >= fromIntegral (bufferSize r) = pure ()
  | otherwise = writeAt' r i a

-- |Index setter. Unsafe version of 'writeAt'.
writeAt' :: (MonadIO m,Storable a,Writable w) => Buffer w a -> Word32 -> a -> m ()
writeAt' r i a = liftIO $ do
  glBindBuffer GL_ARRAY_BUFFER (bufferID r)
  p <- glMapBufferRange GL_ARRAY_BUFFER (fromIntegral $ bufferOffset r) (fromIntegral $ bufferSize r) GL_MAP_WRITE_BIT
  pokeElemOff (castPtr p) (fromIntegral i) a
  () <$ glUnmapBuffer GL_ARRAY_BUFFER
