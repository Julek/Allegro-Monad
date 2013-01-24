{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
module Allegro(Allegro(), Display(), Bitmap(), Colour(), runAllegro, createDisplay, createBitmap, destroyBitmap, withDisplay, putPixel, blit, liftIO, makeColour, clearToColour, black) where

import Control.Monad.State
import Control.Monad.Trans
import Data.Word
import Foreign.C
import System.IO.Error

data Display = Display Word
data Bitmap = Bitmap Word
data Colour = Colour CUChar CUChar CUChar

newtype Allegro a = Allegro (StateT [Word] IO a)
        deriving(Monad, MonadIO)

black = makeColour 0 0 0

foreign import ccall "allegro_init" allegro_init :: IO CInt
foreign import ccall "create_display" create_display :: CInt -> CInt -> IO Word
foreign import ccall "create_bitmap" create_bitmap :: CInt -> CInt -> IO Word
foreign import ccall "destroy_bitmap" destroy_bitmap :: Word -> IO ()
foreign import ccall "get_backbuffer" get_backbuffer :: Word -> IO Word
foreign import ccall "flip_display" flip_display :: Word -> IO ()
foreign import ccall "draw_bitmap" draw_bitmap :: Word -> Word -> CInt -> CInt -> IO ()
foreign import ccall "clear_to_colour" clear_to_colour :: Word -> CUChar -> CUChar -> CUChar -> IO ()
foreign import ccall "put_pixel" put_pixel :: Word -> CInt -> CInt -> CUChar -> CUChar -> CUChar -> IO ()


allegroInit :: IO ()
allegroInit = do
            success <- allegro_init
            when (success == 0) (ioError $ mkIOError illegalOperationErrorType "Unable to initialise Allegro library." Nothing Nothing)

runAllegro :: Allegro a -> IO a
runAllegro (Allegro f) = do
                    allegroInit
                    (ret, toFree) <- runStateT f []
                    mapM_ destroy_bitmap toFree
                    return ret

createDisplay :: Int -> Int -> Allegro Display
createDisplay x y = liftIO . fmap Display $ create_display (fromIntegral x) (fromIntegral y)

withDisplay :: Display -> (Bitmap -> Allegro ()) -> Allegro ()
withDisplay (Display display) f = (liftIO . get_backbuffer $ display) >>= \bmp -> (f . Bitmap $ bmp) >> liftIO (flip_display display)

createBitmap :: Int -> Int -> Allegro Bitmap
createBitmap x y = Allegro $ (lift $ create_bitmap (fromIntegral x) (fromIntegral y)) >>= \b -> modify (b:) >> (return . Bitmap $ b)

destroyBitmap :: Bitmap -> Allegro ()
destroyBitmap (Bitmap bmp) = do
              liftIO . destroy_bitmap $ bmp
              Allegro . modify $ (filter (/=bmp))

blit :: Bitmap -> Bitmap -> Int -> Int -> Allegro ()
blit (Bitmap destination) (Bitmap source) x y = liftIO $ draw_bitmap destination source (fromIntegral x) (fromIntegral y)

clearToColour :: Bitmap -> Colour -> Allegro ()
clearToColour (Bitmap bitmap) (Colour r g b) = liftIO $ clear_to_colour bitmap r g b

putPixel :: Bitmap -> Int -> Int -> Colour -> Allegro ()
putPixel (Bitmap display) x y (Colour r g b) = liftIO $ put_pixel display (fromIntegral x) (fromIntegral y) r g b

makeColour :: Int -> Int -> Int -> Colour
makeColour r g b = Colour (fromIntegral r) (fromIntegral g) (fromIntegral b)
