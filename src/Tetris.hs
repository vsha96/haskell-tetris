module Tetris where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

import TetrisGraphics
import TetrisLogic

---------------
-- | data types
---------------

import TetrisTypes

--------------
-- | constants
--------------

-- game display mode
display :: Display
display = InWindow "Hetris" (windowWidth, windowHeight) (300,0)

-- size of the window
windowWidth :: Int
windowWidth = 720
windowHeight :: Int
windowHeight = 600

windowWidthF :: Float
windowWidthF = fromIntegral windowWidth :: Float
windowHeightF :: Float
windowHeightF = fromIntegral windowHeight :: Float

-- background color
bgColor :: Color
bgColor = black

-- simulation steps per second
fps :: Int
fps = 2
-- write fps = 30 above and ...
-- idea:
--    - add deleted lines count in Tetris (TetrisTypes.hs)
--    - modify deleteLine (TetrisLogic.hs) ...
--    - add difficulty factor in Tetris (TetrisTypes.hs)
--    - modify initTetris, reinitTetris ...
--    - catch the time in updateTetris and use the difficulty factor

-------------------
-- | pure functions
-------------------

drawTetris :: Tetris -> Picture
drawTetris tetris = Pictures $ 
  tetrisMap ++ fShape ++ nextFShape ++ [scorePic, pausePic, endPic]
  where
    border = mapBorder tetris
    picMapBorder = Color white $ Line border
    tetrisMap = drawMap tetris ++ [picMapBorder]
    fShape = drawFShape tetris
    nextFShape = drawNextShape tetris 
      ((-windowWidthF)/2+0.2*windowWidthF) (0.15*windowWidthF)
    scorePic =
      Translate 
        ((-windowWidthF)/2+0.1*windowWidthF) (windowWidthF/2-0.15*windowWidthF)
        $ Scale 0.2 0.2
        $ Color white
        $ Text
        $ "score: " ++ 
        (show (score tetris))
    pausePic =
      if pause tetris then
        Translate 
          ((-windowWidthF)/2+0.1*windowWidthF) ((-0.2)*windowHeightF)
          $ Scale 0.5 0.5
          $ Color rose
          $ Text
          $ "PAUSE"
      else
        Blank
    endPic = 
      if gameEnd tetris then
        Translate 
          ((-windowWidthF)/2) ((-0.2)*windowHeightF)
          $ Scale 0.5 0.5
          $ Color rose
          $ Text
          $ "GAME END"
      else
        Blank

-- update the game world
updateTetris :: Float -> Tetris -> Tetris
updateTetris _ t =
  if (pause t || gameEnd t) then
    t
  else
    movement SDown 0 (-1) t

-- handle pressed buttons
handleEvent :: Event -> Tetris -> Tetris
handleEvent  (EventKey (SpecialKey k) Down _ _) t 
  | gameEnd t && (k == KeySpace) = reinitTetris t
  | gameEnd t = t
  | k == KeySpace= t { pause = not (pause t) }
  | pause t = t
  | k == KeyDown = movement SDown 0 (-1) t
  | k == KeyLeft = movement SLeft (-1) 0 t
  | k == KeyRight= movement SRight 1 0 t
  | k == KeyUp   = rotation t
  | otherwise    = t
handleEvent _ t = t

-- reinitialize the game world
reinitTetris :: Tetris -> Tetris
reinitTetris t = initFallingShapes $ t
  { score = 0
  , pause = False
  , gameEnd = False 
  , blockMap = replicate mapHeight $ replicate mapWidth Nothing}
  where
    mapHeight = height t
    mapWidth = width t 

-- init the tetris
initTetris :: StdGen -> Tetris
initTetris gen = initFallingShapes $ Tetris
  gen
  mapHeight 
  mapWidth 
  initScore
  initPause
  initGameEnd
  initMap 
  initShape
  initNextShape
  initShapePos
  initBSize -- the size of a block
  initMapCoord
  initMargin
  initMapBorder
  initBlockCoord
  where
    mapHeight = 20
    mapWidth = 10
    initScore = 0
    initPause = False
    initGameEnd = False
    initMap = replicate mapHeight $ replicate mapWidth Nothing -- testBlockMap
    initShape = [[Nothing]] -- it doesn't matter at init
    initNextShape = [[Nothing]] -- it doesn't matter at init
    initShapePos = (-1, -1) -- it doesn't matter at init
    -- after that we are working with coords
    heightF = fromIntegral mapHeight :: Float
    widthF = fromIntegral mapWidth :: Float
    initBSize = min blockHeight blockWidth
    blockHeight = (windowHeightF - initMargin*2) / heightF
    blockWidth = (windowWidthF / 2 - initMargin*2) / widthF
    initMapCoord = (initMargin, (-windowHeightF)/2+initMargin)
    initMargin = 20
    initMapBorder = 
      [(mapX,mapY)
      ,(mapX,mapY+initBSize*heightF)
      ,(mapX+initBSize*widthF,mapY+initBSize*heightF)
      ,(mapX+initBSize*widthF,mapY)
      ,(mapX,mapY)]
    mapX = fst initMapCoord
    mapY = snd initMapCoord
    initBlockCoord = blockCoordList initMapCoord mapHeight mapWidth initBSize

-----------------------------
-- main function for this app
-----------------------------

-- Run game. This is the ONLY unpure function
run :: IO ()
run = do

  gen <- newStdGen
  let
    tetris = initTetris gen

  putStr $ "starting TETRIS in Haskell\n"
  putStr $ "block map sides:\n"
  putStr $ (show (width tetris)) ++ " " ++ (show (height tetris)) ++ "\n"
  putStr $ "\n*****CONTROLS*****\n"
  putStr $ "UP - rotate the shape\n"
  putStr $ "LEFT, RIGHT, DOWN - move the shape\n"
  putStr $ "SPACE - pause or restart the game at the end\n\n"

  play display bgColor fps tetris drawTetris handleEvent updateTetris



