module Game (gameSettings, G.getStdGen, G.playGame, centreCoords) where

import Data.Bifunctor (bimap)
import Data.Char (toUpper)
import qualified Terminal.Game as G

data Direction = U | D | L | R | Stop deriving (Eq)

data Player
data Box
data Enemy

data Character t = Character { entityCoords :: !G.Coords
                             , entityDirection :: !Direction }

drawPlayer :: Character Player -> (G.Coords, G.Plane)
drawPlayer character = (entityCoords character, G.color G.Blue G.Vivid $ G.cell 'P')

drawBox :: Character Box -> (G.Coords, G.Plane)
drawBox character = (entityCoords character, G.color G.Green G.Dull $ G.cell 'O')

drawEnemy :: Character Enemy -> (G.Coords, G.Plane)
drawEnemy character = (entityCoords character, G.color G.Red G.Vivid $ G.cell 'X')

drawBorder :: (G.Coords, G.Plane)
drawBorder = do
  let outerBorder = G.box '%' (fst bottomRightBoundary) (snd bottomRightBoundary)
  let innerBlank = bimap (+ 1) (+ 1) topLeftBoundary G.% G.box ' ' (fst bottomRightBoundary - 2) (snd bottomRightBoundary - 2)
  (topLeftBoundary, outerBorder G.& innerBlank)

data State = State { statePlayer :: !(Character Player)
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
initState stdgen = State { statePlayer = initPlayer
                         , stateBox = initBox
                         , stateEnemy = initEnemies
                         , stateIsQuitting = False
                         , stateRandomGen = stdgen
                         }

centreCoords :: G.Coords
centreCoords = (centreCoord (snd topLeftBoundary) (snd bottomRightBoundary), centreCoord (fst topLeftBoundary) (fst bottomRightBoundary))
  where
    centreCoord lower upper = (+) lower $ flip div 2 $ upper - lower

initPlayer :: Character Player
initPlayer = Character { entityCoords = centreCoords, entityDirection = Stop }

initBox :: Character Box
initBox = Character { entityCoords = limitCoords $ bimap (+ 1) (+ 1) centreCoords, entityDirection = Stop }

initEnemies :: [Character Enemy]
initEnemies = mempty

handleEvent :: State -> G.Event -> State
handleEvent state (G.KeyPress key) = handleKeyPress state $ toUpper key
handleEvent state G.Tick = handleTick state

handleTick :: State -> State
handleTick state = do
  let player = statePlayer state
  state { statePlayer = moveCharacter (entityDirection player) player }

handleKeyPress :: State -> Char -> State
handleKeyPress state 'Q' = state { stateIsQuitting = True }
handleKeyPress state 'W' = state { statePlayer = updateDirection U (statePlayer state) }
handleKeyPress state 'S' = state { statePlayer = updateDirection D (statePlayer state) }
handleKeyPress state 'A' = state { statePlayer = updateDirection L (statePlayer state) }
handleKeyPress state 'D' = state { statePlayer = updateDirection R (statePlayer state) }
handleKeyPress state _ = state

render :: State -> G.Plane
render state = do
  let playerPlane = drawPlayer $ statePlayer state
  let boxPlane = drawBox $ stateBox state
  let enemyPlanes = drawEnemy <$> stateEnemy state
  let blank = G.blankPlane (fst bottomRightBoundary) (snd bottomRightBoundary)
  G.mergePlanes blank $ drawBorder : playerPlane : boxPlane : enemyPlanes

shouldQuit :: State -> Bool
shouldQuit = stateIsQuitting

boundaries :: (G.Coords, G.Coords)
boundaries = ((1, 1), (80, 24))

topLeftBoundary :: G.Coords
topLeftBoundary = fst boundaries

bottomRightBoundary :: G.Coords
bottomRightBoundary = snd boundaries

updateDirection :: Direction -> Character t -> Character t
updateDirection direction character@(Character { entityDirection = currentDirection }) = character { entityDirection = if direction == currentDirection then Stop else direction }

moveCharacter :: Direction -> Character t -> Character t
moveCharacter U character = updatePosition (-1, 0) character
moveCharacter D character = updatePosition (1, 0) character
moveCharacter L character = updatePosition (0, -1) character
moveCharacter R character = updatePosition (0, 1) character
moveCharacter _ character = character

limitCoords :: G.Coords -> G.Coords
limitCoords (a, b) = (limitRowCoord a, limitColumnCoord b)
  where
    limitRowCoord a
      | a < snd topLeftBoundary = snd topLeftBoundary
      | a > snd bottomRightBoundary = snd bottomRightBoundary
      | otherwise = a
    limitColumnCoord a
      | a < fst topLeftBoundary = fst topLeftBoundary
      | a > fst bottomRightBoundary = fst bottomRightBoundary
      | otherwise = a

updatePosition :: G.Coords -> Character t -> Character t
updatePosition (velocityX, velocityY) character = do
  let (oldRow, oldColumn) = entityCoords character
  let newCoords = (oldRow + velocityX, oldColumn + velocityY)
  let boundedCoords = limitCoords newCoords
  character { entityCoords = boundedCoords }
