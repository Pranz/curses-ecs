module Event where

import Data.Ecs
import Data.Foldable (forM_)

import UI.NCurses

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Concurrent
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitSuccess)

import Types
import Position

handleEvent :: Event -> Game ()
handleEvent event = do
    player <- fst . head <$> getComponentDataList Player
    case event of
      EventCharacter 'q' ->  liftIO exitSuccess
      EventSpecialKey KeyLeftArrow  -> do
        moveEntity player (negate 1) 0
      EventSpecialKey KeyRightArrow -> do
        moveEntity player 1 0
      EventSpecialKey KeyUpArrow    -> do
        moveEntity player 0 (negate 1)
      EventSpecialKey KeyDownArrow  -> do
        moveEntity player 0 1
      _ -> return ()

demandEvent :: Window -> Curses (Event, EventType)
demandEvent w = do
    mev <- getEvent w Nothing
    case (validateEvent mev)of
      Just (ev, evT) -> return (ev, evT)
      Nothing        -> demandEvent w

validateEvent :: Maybe Event -> Maybe (Event, EventType)
validateEvent mev = do
  ev <- mev
  eventType <- eventType ev
  return (ev, eventType)


eventType :: Event -> Maybe EventType
eventType ev
    | ev `elem` consuming    = Just ConsumesTurn
    | ev `elem` nonconsuming = Just Nonconsuming
    | otherwise = Nothing
  where consuming    = map EventCharacter ['q','k'] ++ map EventSpecialKey [KeyUpArrow, KeyLeftArrow, KeyDownArrow, KeyRightArrow]
        nonconsuming = []
