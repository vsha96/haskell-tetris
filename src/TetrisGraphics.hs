module TetrisGraphics where

import Graphics.Gloss.Interface.Pure.Game

import TetrisTypes

-------------------
-- | pure functions
-------------------

-- make a list of block coords
--    i index is on axisX (from left ro right)
--    j index is on axisY (from bottom to top)
blockCoordList :: Point -> Int -> Int -> Float -> Path
blockCoordList mapCoord height width blockSize = 
  [(x0 + i * blockSize, y0 + j * blockSize) 
    | j<-[0..(heightF-1)], i<-[0..(widthF-1)]]
  where
    heightF = fromIntegral height :: Float
    widthF = fromIntegral width :: Float
    x0 = fst mapCoord
    y0 = snd mapCoord

-- draw a block with size
drawBlock :: Float -> Block -> Picture
drawBlock _ Nothing = Blank
drawBlock bSize (Just c) = Pictures [box, boxBorder]
  where
    box = Color c $ polygon path
    boxBorder = Color white $ Line path
    path = [(0, 0)
           ,(0, bSize)
           ,(bSize, bSize)
           ,(bSize, 0)
           ,(0, 0)]

-- draw a whole shape with size at coord-s
drawShape :: Float -> Shape -> Path -> [Picture]
drawShape bSize shape coordList =
  zipWith
    (\pic (x,y) -> Translate x y pic)
    (map (drawBlock bSize)
    (unzipList shape))
    coordList
  where
    unzipList :: [[a]] -> [a]
    unzipList [] = []
    unzipList (x:xs)= x ++ (unzipList xs)

-- draw a block map
drawMap :: Tetris -> [Picture]
drawMap tetris = drawShape bS bM bC
  where
    bS = blockSize tetris
    bM = blockMap tetris
    bC = blockCoord tetris

-- draw the falling shape
drawFShape :: Tetris -> [Picture]
drawFShape tetris = drawShape bS fS coordList
  where
    fS = fallingShape tetris
    bS = blockSize tetris
    bC = blockCoord tetris
    shapePos = fallingShapePos tetris
    point = bC !! ((fst shapePos) + ((width tetris))*(snd shapePos))
    coordList = blockCoordList point (length fS) (length (fS !! 0)) bS

-- draw the next shape at coords
drawNextShape :: Tetris -> Float -> Float -> [Picture]
drawNextShape tetris x0 y0 = drawShape bS nFS coordList
  where
    nFS = nextFallingShape tetris
    bS = blockSize tetris
    coordList = blockCoordList (x0,y0) (length nFS) (length (head nFS)) bS