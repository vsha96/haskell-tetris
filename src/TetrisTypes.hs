module TetrisTypes where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

---------------
-- | data types
---------------

-- a block in tetris
type Block = Maybe Color

-- the shape consists of blocks
type Shape = [[Block]]

-- shape types
data ShapeType = J | I | O | L | Z | T | S

-- the game - Tetris
data Tetris = Tetris
    { randomGen :: StdGen -- random number generator
    , height :: Int -- in blocks
    , width :: Int -- in blocks
    , score :: Int -- player's game score or points
    , pause :: Bool -- is the game paused
    , gameEnd :: Bool -- is it the end of the game
    , blockMap :: [[Block]] -- matrix of blocks, game map
    , fallingShape :: Shape -- the current falling shape
    , nextFallingShape :: Shape -- -- the next falling shape
    , fallingShapePos :: (Int, Int) -- of falling shape
    , blockSize :: Float -- size of a block in pixels
    , mapCoord :: Point -- coords of the left bottom corner of map
    , margin :: Float -- a margin with windows borders
    , mapBorder :: Path -- border of the map
    , blockCoord :: Path -- list of block coords
    }

--------------
-- | constants
--------------

-- block map for tests
testBlockMap :: [[Block]]
testBlockMap =
  [[Just violet, Just violet, Just violet, Just violet, Just green, Just green, Just green, Just green, Just red, Just red]
  ,[Just green, Just magenta, Just magenta, Just magenta, Just green, Just green, Just green, Just green, Just green, Just red]
  ,[Nothing, Nothing, Just magenta, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just red]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just green]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just green]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just green]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just green]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ]

-------------------
-- | pure functions
-------------------

-- hardcoded shapes by a specific type
shapeFigure :: ShapeType -> Shape
shapeFigure J =
  [[Just red , Just red]
  ,[Nothing  , Just red]
  ,[Nothing , Just red]]
shapeFigure I =
  [[Just green, Just green, Just green, Just green]]
shapeFigure O =
  [[Just blue  , Just blue]
  ,[Just blue  , Just blue]]
shapeFigure L =
  [[Just yellow , Just yellow]
  ,[Just yellow , Nothing]
  ,[Just yellow , Nothing]]
shapeFigure Z =
  [[Just cyan , Just cyan , Nothing]
  ,[Nothing   , Just cyan , Just cyan]]
shapeFigure T = 
  [[Just magenta , Just magenta , Just magenta]
  ,[Nothing      , Just magenta , Nothing]]
shapeFigure S =
  [[Nothing   , Just rose , Just rose]
  ,[Just rose , Just rose , Nothing]]
-- J | I | O | L | Z | T | S
-- palette = [red, green, blue, yellow, cyan, magenta, rose, violet]

shapeTypeByNumber :: Int -> ShapeType
shapeTypeByNumber n | n == 1 = J
                    | n == 2 = I
                    | n == 3 = O
                    | n == 4 = L
                    | n == 5 = Z
                    | n == 6 = T
                    | n == 7 = S
