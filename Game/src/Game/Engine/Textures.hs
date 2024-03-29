{- Textures.hs;

  Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005 - original author
  Garrett Bluma  (gb@garrettbluma.com)     2012 - modified
  Mark Zhan      (fallen.s4e@gmail.com)    2013 - midified

This module is for loading textures

-}

module Game.Engine.Textures
  ( Textures
  , getAndCreateTexturesAll
  , nextTexture
  ) where

import Graphics.Rendering.OpenGL
import Game.Engine.PNG (readPng)
import Data.Word (Word8)
import Data.List ( isSuffixOf, elemIndex )
import Data.Maybe ( fromJust )
import System.Directory ( getDirectoryContents )
import Foreign.Marshal.Alloc (free)
import qualified Control.Exception
import Control.Arrow ( second )
import Debug.Trace
import Data.IORef
import System.IO.Unsafe

type Textures = [(String, TextureObject)]

nextTexture :: Textures -> TextureObject -> TextureObject
nextTexture ts t = f $ elemIndex t ts'
  where f (Just idx) = let idx'  = ((idx + 1) `rem` length ts')
                       in (ts' !! idx')
        f Nothing    = error "nextTexture: texture object must be in the list"
        ts'          = map snd ts

-- | read files and creates a texture only once. It is a memorizing function
getAndCreateTexturesAll :: FilePath -> IO Textures
getAndCreateTexturesAll path = do
  hashPure <- readIORef hash
  case lookup path hashPure of
    Just a  -> return a
    Nothing -> do
      pathes <- getDirectoryContents path
      let pngs          = filter (".png" `isSuffixOf`) pathes
          toTexObj file = readImageC file >>= createTexture
          fullName p    = path ++ "/" ++ p
      texObjs <- mapM (toTexObj . fullName) pngs
      let filtered = filter ((/= Nothing) . snd) $ zip pngs texObjs
          res      = map (second fromJust) filtered
      modifyIORef hash ((path, res):)
      return $ res

-- XXX
hash :: IORef [(FilePath, Textures)]
hash = unsafePerformIO $ newIORef []

-- low level stuff
------------------

-- read the image data
readImageC :: String -> IO (Maybe (Size, PixelData Word8))
readImageC path
  = do putStrLn $ "** [readImageC] Loading image " ++ path
       Control.Exception.catch (readPng path) f
    where
       f :: IOError -> IO (Maybe (Size, PixelData Word8))
       f _ = print ("missing texture: "++path) >> return Nothing

-- creates the texture
createTexture :: (Maybe (Size, PixelData a)) -> IO (Maybe TextureObject)
createTexture (Just ((Size x y), pixels@(PixelData _ _ ptr))) = do
   [texName] <- genObjectNames 1  -- generate our texture.
   --rowAlignment  Unpack $= 1
   textureBinding Texture2D $= Just texName  -- make our new texture the current texture.
   generateMipmap Texture2D $= Enabled
   build2DMipmaps Texture2D RGBA' (fromIntegral x) (fromIntegral y) pixels
   textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
   textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
   textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
   textureFunction $= Modulate
   -- free ptr -- (TGA needs this, PNG doesn't)
   return (Just texName)
createTexture Nothing = return Nothing

