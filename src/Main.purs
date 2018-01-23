module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.JQuery as JQuery
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Timer (setInterval, TIMER)
import Data.Array as Array
import Data.Int
import Data.Maybe (Maybe(..))
import Data.Traversable
import Graphics.Canvas (CANVAS)
import Graphics.Canvas as Canvas
import Math as Math
import Prelude

main = do
  Canvas.tryLoadImage
    "img/turtle-side.JPG"
    (\mturtle -> do
       case mturtle of
         Nothing -> pure unit
         Just turtle -> do
           mcanvas <- Canvas.getCanvasElementById "canvas"
           case mcanvas of
             Nothing -> pure unit
             Just canvas -> do
               width <- Canvas.getCanvasWidth canvas
               height <- Canvas.getCanvasWidth canvas
               log (show width <> " " <> show height)
               ctx <- Canvas.getContext2D canvas
               rescale ctx
               body <- JQuery.select "body"
               turtleX <- newRef defaultTurtleX
               turtleY <- newRef defaultTurtleY
               let redraw = do
                     x <- readRef turtleX
                     y <- readRef turtleY
                     clear width height ctx
                     Canvas.drawImageScale
                       ctx
                       turtle
                       x
                       y
                       (turtleHeight * 1.5)
                       turtleHeight
                     Canvas.restore ctx
               redraw
               JQuery.on
                 "keydown"
                 (\ev j -> do
                    key <- JQuery.getWhich ev
                    log ("Keydown! " <> show (key + 0))
                    case key of
                      37 -> modifyRef turtleX (\x -> x - 10.0)
                      39 -> modifyRef turtleX (\x -> x + 10.0)
                      38 -> modifyRef turtleY (\y -> y - 10.0)
                      40 -> modifyRef turtleY (\y -> y + 10.0)
                      _ -> pure unit
                    redraw)
                 body
               pure unit)

turtleHeight = 100.0

defaultTurtleX = 100.0
defaultTurtleY = 100.0

-- For retina screens:
rescale = Canvas.scale  { scaleX: 2.0, scaleY: 2.0 }

clear w h ctx = Canvas.clearRect ctx { x: 0.0, y: 0.0, w:  w, h:  h}
