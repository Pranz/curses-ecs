module Render where

import Data.Ecs
import UI.NCurses
import Control.Monad.Trans.Class (lift)
import Data.Foldable (forM_)

import Types
import Util

renderWorld :: Game ()
renderWorld = do
    w <- getWindow
    renderables <- getComponentDataList Printable
    forM_ renderables $ \(inst, Graph c) -> do
      (Pos x y) <- getComponentData Position inst
      lift . updateWindow w $ do
        moveCursor (fromIntegral y) (fromIntegral x)
        drawString . return $ c
    lift render
