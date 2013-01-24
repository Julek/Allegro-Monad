import Control.Concurrent
import Control.Monad
import Control.Monad.Random
import Data.IORef
import System.Environment
import System.Exit

import Allegro
import Life
import Render

main :: IO ()
main = do
     args@(~[file]) <- getArgs
     when (length args /= 1) usage
     initBoard <- readBoard file
     boardVar <- newIORef initBoard
     runAllegro $ do
                display <- createDisplay 640 480
                render display initBoard
                liftIO $ threadDelay 100000
                forever $ do
                        board <- liftIO . readIORef $ boardVar
                        board' <- liftIO . turn $ board
                        render display board'
                        liftIO $ writeIORef boardVar board'
                        liftIO $ threadDelay 100000
     return ()

usage :: IO ()
usage = do
      putStrLn "usage: Life input"
      exitFailure
