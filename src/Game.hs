module Game where

data Entity = Entity { entityChar :: !Char
                     , entityPosition :: !Position
                     }

data Position = Position { positionX :: !Integer
                         , positionY :: !Integer
                         } deriving (Eq)
