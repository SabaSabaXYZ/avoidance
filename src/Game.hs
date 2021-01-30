module Game where

import qualified Terminal.Game as G

newtype Vector = Vector { getVector :: (G.Row, G.Column) } deriving (Eq)

instance Semigroup Vector where
  (Vector (x1, y1)) <> (Vector (x2, y2)) = Vector $ (addLimit x1 x2, addLimit y1 y2) where
    addLimit a b
      | a + b > 1 = 1
      | a + b < -1 = -1
      | otherwise = a + b

instance Monoid Vector where
  mempty = Vector (0, 0)

data Direction = U | D | L | R deriving (Eq)

data Player
data Box
data Enemy

data Character t = Character { entityCoords :: !G.Coords
                             , entityVector :: !Vector
                             }

drawPlayer :: Character Player -> G.Plane
drawPlayer _ = G.cell '&'

drawBox :: Character Box -> G.Plane
drawBox _ = G.cell 'O'

drawEnemy :: Character Enemy -> G.Plane
drawEnemy _ = G.cell 'X'

data State = State { stateDirection :: ![Direction]
                   , statePlayer :: !(Character Player)
                   , stateBox :: !(Character Box)
                   , stateEnemy :: ![Character Enemy]
                   , stateIsQuitting :: !Bool
                   }

boundaries :: (G.Coords, G.Coords)
boundaries = ((1, 1), (24, 80))

gameSettings :: G.Game State
gameSettings = G.Game { G.gScreenWidth = fst $ snd boundaries
                      , G.gScreenHeight = snd $ snd boundaries
                      , G.gFPS = 15
                      , G.gInitState = initState
                      , G.gLogicFunction = handleEvent
                      , G.gDrawFunction = render
                      , G.gQuitFunction = shouldQuit
                      }

initState :: State
initState = undefined

handleEvent :: State -> G.Event -> State
handleEvent = undefined

render :: State -> G.Plane
render = undefined

shouldQuit :: State -> Bool
shouldQuit = undefined

directionToVector :: Direction -> Vector
directionToVector U = Vector (0, -1)
directionToVector D = Vector (0, 1)
directionToVector L = Vector (-1, 0)
directionToVector R = Vector (1, 0)
