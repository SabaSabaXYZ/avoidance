module Game (gameSettings, G.getStdGen, G.playGame) where

import Data.Bifunctor (bimap)
import Data.Char (toUpper)
import Data.Maybe (listToMaybe)
import System.Random (Random(..))
import qualified Terminal.Game as G

data Screen = TitleScreen | HelpScreen | GameScreen deriving (Eq)

data Direction = U | D | L | R | Stop deriving (Eq, Bounded, Enum)

instance Random Direction where
  random g = randomR (U, R) g
  randomR (lower, upper) g = case randomR (fromEnum lower, fromEnum upper) g of (r, g') -> (toEnum r, g')

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

drawScore :: State -> (G.Coords, G.Plane)
drawScore state = ((snd bottomRightBoundary, fst topLeftBoundary), G.stringPlane $ "Score: " <> show (stateScore state))

drawTitle :: (G.Coords, G.Plane)
drawTitle = (bimap (flip (-) 3) (flip (-) 6) centreCoords, G.stringPlane "==AVOIDANCE==\n\n\nMove: WASD\nHelp: H\nPlay: P\nQuit: Q")

drawHelp :: (G.Coords, G.Plane)
drawHelp = (topLeftBoundary, G.textBox "Use W, A, S, and D to move up, left, down, and right respectively.\nPress Q during an active game to return to the main menu.\nPress Q while on the main menu to terminate the application.\n\nOBJECTIVE:\n\nPush the box around the screen, making sure that it is not pushed off the edge.\nBox thieves will appear sporadically to steal the box.\nYour objective is to keep the box on-screen for as long as possible.\n\n  P: Player\n  O: Box\n  X: Box Thief\n\nPress any key to return to the main menu." (fst bottomRightBoundary) (snd bottomRightBoundary))

drawBlank :: G.Plane
drawBlank = G.blankPlane (fst bottomRightBoundary) (snd bottomRightBoundary)

data State = State { statePlayer :: !(Character Player)
                   , stateBox :: !(Character Box)
                   , stateEnemy :: ![Character Enemy]
                   , stateDifficulty :: !Int
                   , stateIsQuitting :: !Bool
                   , stateRandomGen :: !G.StdGen
                   , stateScreen :: !Screen
                   , stateScore :: !Int
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
                         , stateDifficulty = 1
                         , stateIsQuitting = False
                         , stateRandomGen = stdgen
                         , stateScreen = TitleScreen
                         , stateScore = 0
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
handleTick state@(State { stateScreen = GameScreen }) = do
  let stateAfterEnemies = handleEnemies state
  let oldPlayer = statePlayer state
  let player = moveCharacter (entityDirection oldPlayer) oldPlayer
  let box = handleCollision player $ handleCollisions (stateEnemy stateAfterEnemies) (stateBox stateAfterEnemies)
  let newScore = stateScore stateAfterEnemies + 1
  if isOutOfBounds box then stateAfterEnemies { stateScreen = TitleScreen }
                       else stateAfterEnemies { statePlayer = resetDirection player, stateBox = box, stateScore = newScore, stateDifficulty = div newScore 200 + 1 }
handleTick state = state

handleEnemies :: State -> State
handleEnemies state = do
  let oldEnemies = stateEnemy state
  let randomGen1 = stateRandomGen state
  let maxEnemies = stateDifficulty state
  let (enemiesToCreate, randomGen2) = G.getRandom (0, maxEnemies - length oldEnemies) randomGen1
  let (newRandomEnemies, randomGen3) = createEnemies enemiesToCreate (oldEnemies, randomGen2)
  let updatedEnemies = moveEnemies newRandomEnemies
  state { stateEnemy = updatedEnemies, stateRandomGen = randomGen3 }

moveEnemies :: [Character Enemy] -> [Character Enemy]
moveEnemies = filter (not . isOutOfBounds) . fmap (\character -> moveCharacter (entityDirection character) character)

createEnemies :: Int -> ([Character Enemy], G.StdGen) -> ([Character Enemy], G.StdGen)
createEnemies 0 (enemies, randomGen) = (enemies, randomGen)
createEnemies enemiesToCreate (enemies, randomGen) = let (enemy, newRandomGen) = createEnemy randomGen in createEnemies (enemiesToCreate - 1) (enemy : enemies, newRandomGen)

createEnemy :: G.StdGen -> (Character Enemy, G.StdGen)
createEnemy randomGen = do
  let (randomDirection, randomGen1) = random randomGen
  let (position, randomGen2) = enemyStartPosition randomGen1 randomDirection
  (Character { entityCoords = position, entityDirection = randomDirection }, randomGen2)

enemyStartPosition :: G.StdGen -> Direction -> (G.Coords, G.StdGen)
enemyStartPosition randomGen U = let (column, newRandomGen) = G.getRandom (fst topLeftBoundary, fst bottomRightBoundary) randomGen in ((snd bottomRightBoundary, column), newRandomGen)
enemyStartPosition randomGen D = let (column, newRandomGen) = G.getRandom (fst topLeftBoundary, fst bottomRightBoundary) randomGen in ((snd topLeftBoundary, column), newRandomGen)
enemyStartPosition randomGen L = let (row, newRandomGen) = G.getRandom (snd topLeftBoundary, snd bottomRightBoundary) randomGen in ((row, fst bottomRightBoundary), newRandomGen)
enemyStartPosition randomGen R = let (row, newRandomGen) = G.getRandom (snd topLeftBoundary, snd bottomRightBoundary) randomGen in ((row, fst topLeftBoundary), newRandomGen)

handleKeyPress :: State -> Char -> State
handleKeyPress state@(State { stateScreen = TitleScreen }) 'Q' = state { stateIsQuitting = True }
handleKeyPress state@(State { stateScreen = TitleScreen }) 'P' = state { statePlayer = initPlayer, stateBox = initBox, stateEnemy = initEnemies, stateDifficulty = 1, stateScreen = GameScreen, stateScore = 0 }
handleKeyPress state@(State { stateScreen = TitleScreen }) 'H' = state { stateScreen = HelpScreen }
handleKeyPress state@(State { stateScreen = HelpScreen }) _ = state { stateScreen = TitleScreen }
handleKeyPress state@(State { stateScreen = GameScreen }) 'Q' = state { stateScreen = TitleScreen }
handleKeyPress state@(State { stateScreen = GameScreen }) 'W' = state { statePlayer = updateDirection U (statePlayer state) }
handleKeyPress state@(State { stateScreen = GameScreen }) 'S' = state { statePlayer = updateDirection D (statePlayer state) }
handleKeyPress state@(State { stateScreen = GameScreen }) 'A' = state { statePlayer = updateDirection L (statePlayer state) }
handleKeyPress state@(State { stateScreen = GameScreen }) 'D' = state { statePlayer = updateDirection R (statePlayer state) }
handleKeyPress state _ = state

render :: State -> G.Plane
render state@(State { stateScreen = TitleScreen }) = G.mergePlanes drawBlank $ drawTitle : drawScore state : []
render state@(State { stateScreen = HelpScreen }) = G.mergePlanes drawBlank $ drawHelp : drawScore state : []
render state@(State { stateScreen = GameScreen }) = do
  let playerPlane = drawPlayer $ statePlayer state
  let boxPlane = drawBox $ stateBox state
  let enemyPlanes = drawEnemy <$> stateEnemy state
  G.mergePlanes drawBlank $ drawBorder : drawScore state : playerPlane : boxPlane : enemyPlanes

shouldQuit :: State -> Bool
shouldQuit = stateIsQuitting

boundaries :: (G.Coords, G.Coords)
boundaries = ((1, 1), (80, 24))

topLeftBoundary :: G.Coords
topLeftBoundary = fst boundaries

bottomRightBoundary :: G.Coords
bottomRightBoundary = snd boundaries

resetDirection :: Character Player -> Character Player
resetDirection character = character { entityDirection = Stop }

updateDirection :: Direction -> Character Player -> Character Player
updateDirection direction character = character { entityDirection = direction }

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
updatePosition (velocityRow, velocityColumn) character = do
  let (oldRow, oldColumn) = entityCoords character
  let newCoords = (oldRow + velocityRow, oldColumn + velocityColumn)
  let boundedCoords = limitCoords newCoords
  character { entityCoords = boundedCoords }

willCollide :: Direction -> G.Coords -> G.Coords -> Bool
willCollide Stop _ _ = False
willCollide _ first second = first == second

handleCollisions :: [Character Enemy] -> Character Box -> Character Box
handleCollisions enemies box = do
  let matchingEnemy = listToMaybe $ filter (\enemy -> willCollide (entityDirection enemy) (entityCoords enemy) (entityCoords box)) enemies
  case matchingEnemy of
    Nothing -> box
    Just enemy -> moveCharacter (entityDirection enemy) box

handleCollision :: Character t -> Character Box -> Character Box
handleCollision (Character { entityCoords = coords, entityDirection = direction }) box
  | willCollide direction coords (entityCoords box) = moveCharacter direction box
  | otherwise = box

isOutOfBounds :: Character t -> Bool
isOutOfBounds (Character { entityCoords = (row, column) }) = row == fst topLeftBoundary || column == snd topLeftBoundary || row == snd bottomRightBoundary || column == fst bottomRightBoundary
