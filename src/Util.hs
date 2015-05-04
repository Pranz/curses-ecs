module Util where

import Data.Ecs
import UI.NCurses
import Control.Monad.Trans.State
import Data.Array.IO
import Control.Applicative ((<$>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Types

getWindow :: Game Window
getWindow = fmap (window.additionalState) get

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap

putWall :: Int -> Int -> Game ()
putWall x y = do
    (wall   . additionalState <$> get) >>= liftIO . (\a -> writeArray a (fromIntegral x, fromIntegral y) True)
    (window . additionalState <$> get) >>= lift   . flip updateWindow (
        moveCursor (fromIntegral y) (fromIntegral x) >>
        drawString "#")

destroyWall :: Int -> Int -> Game ()
destroyWall x y = do
    (wall   . additionalState <$> get) >>= liftIO . (\a -> writeArray a (fromIntegral x, fromIntegral y) False)
    (window . additionalState <$> get) >>= lift   . flip updateWindow (
        moveCursor (fromIntegral y) (fromIntegral x) >>
        drawString " ")

