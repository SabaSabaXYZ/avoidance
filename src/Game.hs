module Game where

import Data.Bifunctor (bimap)
import Data.Char (toUpper)
import qualified Terminal.Game as G

data Direction = U | D | L | R deriving (Eq)

data Player
data Box
data Enemy

newtype Character t = Character { entityCoords :: G.Coords }

drawPlayer :: Character Player -> (G.Coords, G.Plane)
drawPlayer character = (entityCoords character, G.cell '&')

drawBox :: Character Box -> (G.Coords, G.Plane)
drawBox character = (entityCoords character, G.cell 'O')

drawEnemy :: Character Enemy -> (G.Coords, G.Plane)
drawEnemy character = (entityCoords character, G.cell 'X')

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
initPlayer = Character { entityCoords = centreCoords }

initBox :: Character Box
initBox = Character { entityCoords = limitCoords $ bimap (+ 1) (+ 1) centreCoords }

initEnemies :: [Character Enemy]
initEnemies = mempty

handleEvent :: State -> G.Event -> State
handleEvent state G.Tick = handleTick state
handleEvent state (G.KeyPress key) = handleKeyPress state $ toUpper key

handleTick :: State -> State
handleTick state = state

handleKeyPress :: State -> Char -> State
handleKeyPress state 'Q' = state { stateIsQuitting = True }
handleKeyPress state 'W' = state { statePlayer = moveCharacter U (statePlayer state) }
handleKeyPress state 'S' = state { statePlayer = moveCharacter D (statePlayer state) }
handleKeyPress state 'A' = state { statePlayer = moveCharacter L (statePlayer state) }
handleKeyPress state 'D' = state { statePlayer = moveCharacter R (statePlayer state) }
handleKeyPress state _ = state

render :: State -> G.Plane
render state = do
  let playerPlane = drawPlayer $ statePlayer state
  let boxPlane = drawBox $ stateBox state
  let enemyPlanes = drawEnemy <$> stateEnemy state
  let blank = G.blankPlane (fst bottomRightBoundary) (snd bottomRightBoundary)
  G.mergePlanes blank $ playerPlane : boxPlane : enemyPlanes

shouldQuit :: State -> Bool
shouldQuit = stateIsQuitting

boundaries :: (G.Coords, G.Coords)
boundaries = ((1, 1), (24, 80))

topLeftBoundary :: G.Coords
topLeftBoundary = fst boundaries

bottomRightBoundary :: G.Coords
bottomRightBoundary = snd boundaries

moveCharacter :: Direction -> Character t -> Character t
moveCharacter U character = updatePosition (0, -1) character
moveCharacter D character = updatePosition (0, 1) character
moveCharacter L character = updatePosition (-1, 0) character
moveCharacter R character = updatePosition (1, 0) character

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

updatePosition :: G.Coords -> Character t -> Character t
updatePosition (velocityX, velocityY) character = do
  let (oldRow, oldColumn) = entityCoords character
  let newCoords = (oldRow + velocityX, oldColumn + velocityY)
  let boundedCoords = limitCoords newCoords
  character { entityCoords = boundedCoords }
