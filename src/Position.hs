module Position where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, unless)

import Data.Maybe (mapMaybe)
import qualified Data.Foldable (all)
import Data.Ecs
import UI.NCurses

import Types
import Util

moveEntity :: Instance -> Int -> Int -> Game ()
moveEntity inst xx yy = do
    (Pos x y) <- getComponentData Position inst
    let newPos@(Pos newX newY) = Pos (x + xx) (y + yy)
    positionables <- fmap (map fst . filter ((\(Pos x' y') -> newX == x' && newY == y') . snd)) $ getComponentDataList Position 
    (interactables, collisionables) <- do
      inters <- fmap (mapMaybe (\(i,md) -> fmap (i,) md)) . sequence 
        $ fmap (\i -> fmap (i,) $ mgetComponentData Interactable i) positionables
      collis <- fmap (mapMaybe (\(i,md) -> fmap (i,) md)) . sequence 
        $ fmap (\i -> fmap (i,) $ mgetComponentData Collision i)    positionables
      return (inters, collis)
    mapM_ (\(self, Interacts f) -> f self inst) interactables
    collides <- mapM (\(self, Collides col) -> col self) collisionables
    w <- getWindow
    unless (foldr (||) False collides) $ do
      lift . updateWindow w $ do
        moveCursor (fromIntegral y) (fromIntegral x)
        drawString " "
      modifyComponentOf Position inst (const newPos)
