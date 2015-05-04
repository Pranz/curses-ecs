module Types where

import Data.Ecs
import Data.Map (Map)
import UI.NCurses
import Control.Monad.Trans.State
import Control.Concurrent

data Component = Player | HasStats | Interactable | Collision | Position | Printable
              deriving(Eq, Enum, Ord, Bounded, Show, Read)
data EventType = ConsumesTurn | Nonconsuming

data ExitCode = ExitSuccess
              deriving (Eq, Show, Read)

type Game a = StateT (World' Component) Curses a

instance System Component Curses where
    data ComponentData Component   = IsPlayer | Stats Int Int Int Int | Interacts (Instance -> Instance -> Game ()) | Collides (Instance -> Game Bool) | Pos Int Int | Graph Char
    data AdditionalState Component = AdditionalState {window :: Window}
    
    system _ _ _ = return ()


