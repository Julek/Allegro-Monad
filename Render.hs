module Render where

import Control.Monad
import Data.HashTable.IO (toList)

import Allegro
import Life

render :: Display -> Board -> Allegro ()
render display board = do
             buffer <- createBitmap 640 480
             clearToColour buffer black
             list <- liftIO $ fmap (map fst) (toList board)
             let onScreen = map (\(x, y) -> (fromInteger $ x + 320,fromInteger $ y + 240)) . filter (\(x,y) -> (abs x) <= 320 && (abs y) <= 240) $ list
             mapM_ (\(x,y) -> putPixel buffer x y (makeColour 255 255 255)) onScreen
             withDisplay display $ \screen -> do
                         blit screen buffer 0 0
             destroyBitmap buffer
                                                         
