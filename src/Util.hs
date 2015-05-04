module Util where

import Data.Ecs
import UI.NCurses
import Control.Monad.Trans.State

import Types

getWindow :: Game Window
getWindow = fmap (window.additionalState) get