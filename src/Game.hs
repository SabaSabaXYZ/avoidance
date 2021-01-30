module Game where

import Data.Bifunctor (bimap)
import qualified Terminal.Game as G

newtype Velocity = Velocity { getVelocity :: (G.Row, G.Column) } deriving (Eq)

instance Semigroup Velocity where
  (Velocity (x1, y1)) <> (Velocity (x2, y2)) = Velocity $ (addLimit x1 x2, addLimit y1 y2) where
    addLimit a b
      | a + b > 1 = 1
      | a + b < -1 = -1
      | otherwise = a + b

instance Monoid Velocity where
  mempty = Velocity (0, 0)

data Direction = U | D | L | R deriving (Eq)

data Player
data Box
data Enemy

data Character t = Character { entityCoords :: !G.Coords
                             , entityVelocity :: !Velocity
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
                   , stateRandomGen :: !G.StdGen
                   }

gameSettings :: G.StdGen -> G.Game State
gameSettings stdgen = G.Game { G.gScreenWidth = fst bottomRightBoundary
                             , G.gScreenHeight = snd bottomRightBoundary
                             , G.gFPS = 15
                             , G.gInitState = initState stdgen
                             , G.gLogicFunction = handleEvent
                             , G.gDrawFunction = render
                             , G.gQuitFunction = shouldQuit
                             }

initState :: G.StdGen -> State
initState stdgen = State { stateDirection = []
                         , statePlayer = initPlayer
                         , stateBox = initBox
                         , stateEnemy = initEnemies
                         , stateIsQuitting = False
                         , stateRandomGen = stdgen
                         }

centreCoords :: G.Coords
centreCoords = bimap (+ fst topLeftBoundary) (+ snd topLeftBoundary) $ fmap (flip div 2) (fst bottomRightBoundary - fst topLeftBoundary, snd bottomRightBoundary - snd topLeftBoundary)

initPlayer :: Character Player
initPlayer = Character { entityCoords = centreCoords, entityVelocity = mempty }

initBox :: Character Box
initBox = Character { entityCoords = limitCoords $ bimap (+ 1) (+ 1) centreCoords, entityVelocity = mempty }

initEnemies :: [Character Enemy]
initEnemies = mempty

handleEvent :: State -> G.Event -> State
handleEvent = undefined

render :: State -> G.Plane
render = undefined

shouldQuit :: State -> Bool
shouldQuit = stateIsQuitting

boundaries :: (G.Coords, G.Coords)
boundaries = ((1, 1), (24, 80))

topLeftBoundary :: G.Coords
topLeftBoundary = fst boundaries

bottomRightBoundary :: G.Coords
bottomRightBoundary = snd boundaries

directionToVelocity :: Direction -> Velocity
directionToVelocity U = Velocity (0, -1)
directionToVelocity D = Velocity (0, 1)
directionToVelocity L = Velocity (-1, 0)
directionToVelocity R = Velocity (1, 0)

limitCoords :: G.Coords -> G.Coords
limitCoords (a, b) = (limitRowCoord a, limitColumnCoord b)
  where
    limitRowCoord a
      | a < fst topLeftBoundary = fst topLeftBoundary
      | a > fst bottomRightBoundary = fst bottomRightBoundary
      | otherwise = a
    limitColumnCoord a
      | a < snd topLeftBoundary = snd topLeftBoundary
      | a > snd bottomRightBoundary = snd bottomRightBoundary
      | otherwise = a

updatePosition :: Character t -> Character t
updatePosition character = do
  let Velocity (velocityX, velocityY) = entityVelocity character
  let (oldRow, oldColumn) = entityCoords character
  let newCoords = (oldRow + velocityX, oldColumn + velocityY)
  let boundedCoords = limitCoords newCoords
  character { entityCoords = boundedCoords }
