import UI.NCurses
import Data.Ecs
import Data.Map
import Data.Foldable (forM_)
import Data.Array.IO
import Data.Char
import Data.Word
import Data.Function (on)
import Control.Applicative ((<$>))
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad (void)
import Control.Concurrent
import Control.Monad.IO.Class

import System.Exit

import Types
import Render
import Event
import Util

main :: IO ()
main = do
    putStrLn "Starting Game"
    mvar <- newEmptyMVar
    collisionMap <- newArray ((fromIntegral 0, fromIntegral 0),(fromIntegral 128,fromIntegral 128)) False :: IO (IOUArray (Word8,Word8) Bool)
    runCurses $ do
      setEcho False
      setCursorMode CursorInvisible
      w <- newWindow 0 0 0 0
      void $ flip execStateT (initWorld (AdditionalState w collisionMap)) (initGame >> gameLoop)
    return ()

initGame :: Game ()
initGame = do
    mkEntity [(Player, IsPlayer), (Printable, Graph '@'), (Position, Pos 5 5)]
    mkEntity [(Interactable, Interacts (\self other ->
               modifyComponentOf Printable self (\(Graph c) -> (Graph . chr . (+ 1) . ord) c)))
             ,(Position, Pos 7 7)
             ,(Printable, Graph 'k')
             ,(Collision, Collides (const $ return True))
             ]
    wallMap <- wall . additionalState <$> get
    forM_ [0..20] $ \i ->
        putWall i 10
    return ()

gameLoop :: Game ()
gameLoop = do
    renderWorld
    (ev,evType) <- get >>= lift . demandEvent . window . additionalState
    handleEvent ev
    gameLoop

mkEntity :: [(Component, ComponentData Component)] -> Game ()
mkEntity ls = createEntity ls >>= (\i -> forM_ ls (\(c,cd) -> hook i c cd))
  where hook _ _ _ = return ()
