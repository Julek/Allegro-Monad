{-# LANGUAGE TupleSections #-}
module Life where

import Control.Arrow
import Control.Monad
import Data.HashTable.IO as H (LinearHashTable, new, insert, toList, fromList, lookup)
import Data.Int
import Data.List (nub)
import Data.Maybe

type Cell = (Integer, Integer)
type Board = LinearHashTable Cell ()

turn :: Board -> IO Board
turn board = do
     cells <- fmap (map fst) (toList board)
     let active = nub $ concatMap surround cells
     cells' <- fmap (mapMaybe id) (mapM (runCell board) active)
     fromList . map (,()) $ cells'

runCell :: Board -> Cell -> IO (Maybe Cell)
runCell board cell = do
        alive <- isAlive board cell
        if alive
        then doAlive board cell
        else doDead board cell

doAlive :: Board -> Cell -> IO (Maybe Cell)
doAlive board cell = do
        num <- numberSurround board cell
        if num < 2
        then return Nothing
        else if num < 4
             then return . Just $ cell
             else return Nothing

doDead :: Board -> Cell -> IO (Maybe Cell)
doDead board cell = do
       num <- numberSurround board cell
       if num == 3
       then return . Just $ cell
       else return Nothing

isAlive :: Board -> Cell -> IO Bool
isAlive  = curry $ fmap isJust . uncurry H.lookup

numberSurround :: Board -> Cell -> IO Int
numberSurround board cell = fmap (length . filter isJust) (mapM (H.lookup board) surr)
               where surr = surround cell

surround :: Cell -> [Cell]
surround c = map (flip ($) c) transforms
         where funcs = [id, (+1), flip (-) 1]
               transforms = tail [x *** y| x <- funcs, y <- funcs]

printBoard :: Board -> IO ()
printBoard = toList >=> print . map fst

readBoard :: FilePath -> IO Board
readBoard f = do
          boardList <- fmap read (readFile f) :: IO [(Integer, Integer)] 
          board <- new
          mapM_ (flip (insert board) ()) boardList
          return board
