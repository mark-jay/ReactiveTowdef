{- PNG.hs; Garrett Bluma (gb@garrettbluma.com) 2012 -}

module Game.Engine.PNG where

import Codec.Image.PNG
import Data.Array.Storable

import Data.Word ( Word8, Word32 )
import Data.Int (Int32)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (nullPtr, Ptr())
import Graphics.Rendering.OpenGL

readPng :: FilePath -> IO (Maybe (Size, PixelData Word8))
readPng path =  do
  image <- loadPNGFile path

  case image of
    Left msg   -> do print msg
                     return Nothing

    Right img  -> do let (w,h)   = dimensions img
                     let (w2,h2) = (fromIntegral w, fromIntegral h)
                     -- print (w,h)
                     let d       = (imageData img)
                     pixels      <- withStorableArray d getPixelData
                     -- touchStorableArray d
                     return $ Just ((Size w2 h2), pixels)


getPixelData :: Ptr Word8 -> IO (PixelData Word8)
getPixelData ptr =
  return (PixelData RGB UnsignedByte ptr)
