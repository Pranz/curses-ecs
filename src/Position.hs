module Position where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, unless, guard, forM)
import Control.Applicative
import Control.Monad.IO.Class

import Data.Maybe (mapMaybe)
import qualified Data.Foldable as F (all, forM_)
import Data.Ecs
import Data.Array.IO

import UI.NCurses

import Types
import Util

moveEntity :: Instance -> Int -> Int -> Game ()
moveEntity inst xx yy = do
    (Pos x y) <- getComponentData Position inst
    let newPos@(Pos newX newY) = Pos (x + xx) (y + yy)
    entitiesAtSamePosition <- getComponentDataList Position <$$> \ls -> do
        (ent, Pos x' y') <- ls
        guard (x' == newX && y' == newY)
        return ent
    (interactables, collisionables) <- do
        inters <- fmap (mapMaybe (\(i,md) -> fmap (i,) md)) . sequence 
            $ fmap (\i -> fmap (i,) $ mgetComponentData Interactable i) entitiesAtSamePosition
        collis <- fmap (mapMaybe (\(i,md) -> fmap (i,) md)) . sequence 
            $ fmap (\i -> fmap (i,) $ mgetComponentData Collision i)    entitiesAtSamePosition
        return (inters, collis)
    F.forM_ interactables
        (\(self, Interacts f) -> f self inst)
    collides <- forM collisionables
        (\(self, Collides col) -> col self)
    spaceOccupied <- ((wall.additionalState) <$> get) >>= liftIO . flip readArray (fromIntegral newX, fromIntegral newY)
    unless (any id collides || spaceOccupied) $ do
        w <- getWindow
        lift . updateWindow w $ do
            moveCursor (fromIntegral y) (fromIntegral x)
            drawString " "
        modifyComponentOf Position inst (const newPos)
