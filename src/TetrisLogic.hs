module TetrisLogic where

import System.Random
import Data.List
import Data.Maybe

import TetrisTypes

---------------
-- | data types
---------------

-- a direction of shape movement
data Side = SLeft | SRight | SDown deriving Eq

--------------------
-- | move and rotate
--------------------

-- try to move in one direction with modification of falling shape coords
--    and place it if it's needed
movement :: Side -> Int -> Int -> Tetris -> Tetris
movement side dx dy t = case side of
  SDown ->
    if (availableMove side t) then
        t { fallingShapePos = moveShape t dx dy }
    else
      newShape $ placeShape t
  _     ->
    if (availableMove side t) then
      t { fallingShapePos = moveShape t dx dy}
    else
      t

-- can the shape move in one direction
availableMove :: Side -> Tetris -> Bool
availableMove side t = case side of
  SDown ->
    if y > 0 then
      checkMove t (x, y - 1)
    else
      False
  SLeft ->
    if x > 0 then
      checkMove t (x - 1, y)
    else
      False
  SRight ->
    if x + wFS < width t then
      checkMove t (x + 1, y)
    else
      False
  where
    fS  = fallingShape t
    wFS = length $ head fS
    (x, y) = fallingShapePos t
    bM = blockMap t

-- is there a place to put the block here
comparison :: Block -> Block -> Bool
comparison _ Nothing = True
comparison Nothing _ = True
comparison _ _       = False

-- check specific blocks for a free space
checkMove :: Tetris -> (Int, Int) -> Bool
checkMove t (x, y) =
  and $ map and $ zipWith
    (zipWith comparison) 
    fS
    $ map (drop x . take (wFS + x))
      $ (drop (y) . take (hFS + y)) bM
  where
    fS  = fallingShape t
    hFS = length $ fS
    wFS = length $ head fS
    bM = blockMap t

-- move the falling shape
moveShape :: Tetris -> Int -> Int -> (Int, Int)
moveShape t x y = (mod (x0+x) (width t), mod (y0+y) (height t))
  where
    (x0, y0) = fallingShapePos t

-- check space for rotated shape
checkRotation :: Tetris -> Bool
checkRotation t =
  if x + wFS - 1 < width t && y + hFS - 1 < height t then
    and $ map and $ zipWith
      (zipWith comparison) 
      fS
      $ map (drop x . take (wFS + x))
        $ (drop (y) . take (hFS + y)) bM
  else
    False
  where
    (x, y) = fallingShapePos t
    fS  = rotateShape $ fallingShape t
    hFS = length $ fS
    wFS = length $ head fS
    bM = blockMap t

-- try to rotate the falling shape
--    and place it if it's needed
rotation :: Tetris -> Tetris
rotation t = 
  if checkRotation t then
    if (availableMove SDown t { fallingShape = rotateShape fS }) then
      t { fallingShape = rotateShape fS }
    else
      newShape $ placeShape $ t { fallingShape = rotateShape fS }
  else
    t
  where
    fS = fallingShape t

-- rotate the falling shape
rotateShape :: Shape -> Shape
rotateShape shape = map reverse $ transpose shape

---------------------------
-- | place a shape, a block
---------------------------

-- place the falling shape on block map
placeShape :: Tetris -> Tetris
placeShape t = deleteLine t { blockMap = zipWith placeBlock mapFS bM}
  where
    bM = blockMap t
    fS = fallingShape t
    hFS = length $ fS
    wFS = length $ head fS
    (x, y) = fallingShapePos t
    extendFS = 
      map
        ((replicate x Nothing)++) -- to add Nothing from left
        (map (++(replicate ((width t) - x - wFS) Nothing)) fS) -- to add Nothing from right
    nothingRow = (replicate (width t) Nothing) -- row with the size of map width
    mapFS = 
      replicate y nothingRow ++
      extendFS ++
      replicate ((height t) - y - hFS) nothingRow

-- fill blocks with specific colour
placeBlock :: [Block] -> [Block] -> [Block]
placeBlock [] [] = []
placeBlock ((Just c) : xs) (Nothing : ys) = (Just c) : placeBlock xs ys
placeBlock (Nothing  : xs) (smt : ys) = smt : placeBlock xs ys

------------------------
-- | generate new shapes
------------------------

-- Random numbers range.
range :: (Int, Int)
range = (1, 7)

-- generate a new falling shape and the next shape
--    and check game end
--    idea: add uneven shape generation, for a better gaming experience
newShape :: Tetris -> Tetris
newShape t = 
  if checkGameEnd t { fallingShape = newFS, fallingShapePos = newFSP } then
    t { gameEnd = True }
  else
    t
      { randomGen = newGen
      , fallingShape = newFS
      , nextFallingShape = newNextFS
      , fallingShapePos = newFSP}
  where
    (newn, newGen) = randomR range (randomGen t)
    newFS = nextFallingShape t
    hFS = length $ newFS
    wFS = length $ head newFS
    newFSP = (div (width t - wFS) 2, (height t) - hFS)
    newNextFS = shapeFigure $ shapeTypeByNumber newn

-- init the current and the next falling shape 
initFallingShapes :: Tetris -> Tetris
initFallingShapes = newShape . newShape

--------------------------------------
-- | game end, delete lines, add score
--------------------------------------

-- it is end of the game, if we can't place a new falling shape
checkGameEnd :: Tetris -> Bool
checkGameEnd t = not $ checkMove t newFSP
  where
    newFS = fallingShape t
    newFSP = fallingShapePos t

-- check there is a line and delete it
deleteLine :: Tetris -> Tetris
deleteLine t = t
  { blockMap = zipWithDelete linesToDelete bM ++ replicate lineCount nothingRow
  , score = addScore prevScore lineCount}
  where
    prevScore = score t
    bM = blockMap t
    nothingRow = (replicate (width t) Nothing)
    linesToDelete = (map checkLineToDelete bM)
    lineCount = length (filter (==True) linesToDelete) -- count of lines to delete

-- add game points to the score
addScore :: Int -> Int -> Int
addScore prevScore lineCount = prevScore + addPoints
  where
    addPoints = case lineCount of
      1 -> 100
      2 -> 300
      3 -> 700
      4 -> 1500
      _ -> 0

-- if there is True in position then delete line if blocks
zipWithDelete :: [Bool] -> [[Block]] -> [[Block]]
zipWithDelete [] [] = []
zipWithDelete (delete : bs) (x:xs) | delete = (zipWithDelete bs xs)
                                   | otherwise = x : (zipWithDelete bs xs)

-- is the line filled
checkLineToDelete :: [Block] -> Bool
checkLineToDelete = and . map isJust