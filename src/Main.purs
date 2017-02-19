module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (setInterval, TIMER)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Ref
import Data.Maybe(Maybe(..))
import Graphics.Canvas (CANVAS)
import Graphics.Canvas as Canvas
import Math as Math
import Prelude

main :: Eff (console :: CONSOLE, canvas :: CANVAS, timer :: TIMER, ref :: REF) Unit
main = do
  mcanvas <- Canvas.getCanvasElementById "canvas"
  case mcanvas of
    Nothing -> pure unit
    Just canvas -> do
      width <- Canvas.getCanvasWidth canvas
      height <- Canvas.getCanvasWidth canvas
      log ("Width " <> show width <> ", height " <> show height)
      ctx <- Canvas.getContext2D canvas
      ref <- newRef 0.0
      setInterval
        50
        (do r <- readRef ref
            Canvas.clearRect ctx { x: 0.0, y: 0.0, w: width, h: height }
            Canvas.save ctx
            Canvas.beginPath ctx
            Canvas.arc ctx {x: 50.0, y: 50.0, r: 20.0 * Math.sin r, start: 0.0, end: Math.pi*2.0}
            Canvas.closePath ctx
            Canvas.fill ctx
            Canvas.restore ctx
            writeRef ref (if r < Math.pi then Math.min (r + 0.1) Math.pi else 0.0))
      pure unit
