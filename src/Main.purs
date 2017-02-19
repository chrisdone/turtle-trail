module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe(Maybe(..))
import Graphics.Canvas (CANVAS)
import Graphics.Canvas as Canvas
import Math as Math
import Prelude

main :: Eff (console :: CONSOLE, canvas :: CANVAS) Unit
main = do
  mcanvas <- Canvas.getCanvasElementById "canvas"
  case mcanvas of
    Nothing -> pure unit
    Just canvas -> do
      width <- Canvas.getCanvasWidth canvas
      height <- Canvas.getCanvasWidth canvas
      log ("Width " <> show width <> ", height " <> show height)
      ctx <- Canvas.getContext2D canvas
      Canvas.beginPath ctx
      Canvas.arc ctx {x: 50.0, y: 50.0, r: 20.0, start: 0.0, end: Math.pi*2.0}
      Canvas.arc ctx {x: 100.0, y: 50.0, r: 20.0, start: 0.0, end: Math.pi*2.0}
      Canvas.closePath ctx
      Canvas.fill ctx
      pure unit
