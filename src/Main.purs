module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Timer (setInterval, TIMER)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable
import Graphics.Canvas (CANVAS)
import Graphics.Canvas as Canvas
import Math as Math
import Prelude

type Circle =
  { x  :: Number -- ^ X position in pixels.
  , y  :: Number -- ^ Y position in pixels.
  , xv :: Number -- ^ X velocity in pixels per frame.
  , yv :: Number -- ^ Y velocity in pixels per frame.
  , r  :: Number -- ^ Radius in pixels.
  , c  :: String -- ^ Color.
  }

main = do
  mcanvas <- Canvas.getCanvasElementById "canvas"
  case mcanvas of
    Nothing -> pure unit
    Just canvas -> do
      width <- Canvas.getCanvasWidth canvas
      height <- Canvas.getCanvasWidth canvas
      log (show width <> " " <> show height)
      ctx <- Canvas.getContext2D canvas
      ballRefs <-
        traverse
          (\x -> newRef x)
          [
            { i: 0, x: 20.0, y: 30.0, r: 5.0, xv: 2.0, yv: 0.0, c: "#ff90ff", mass: 1.0 }
          , { i: 1, x: 80.0, y: 50.0, r: 5.0, xv: -5.0, yv: 0.0, c: "#55ff00", mass: 1.0 }

          , { i: 2, x: 20.0, y: 50.0, r: 5.0, xv: 1.0, yv: 0.0, c: "#ff0000", mass: 1.0 }
          , { i: 3, x: 50.0, y: 50.0, r: 5.0, xv: -4.0, yv: 0.075, c: "#00ffaa", mass: 1.0 }

          , { i: 4, x: 20.0, y: 100.0, r: 5.0, xv: 5.0, yv: 0.0, c: "#0000ff", mass: 1.0 }
          , { i: 5, x: 85.0, y: 90.0, r: 10.0, xv: -3.0, yv: 0.0, c: "#ff00ff", mass: 2.0 }

          , { i: 6, x: 65.0, y: 70.0, r: 5.0, xv: 0.1, yv: 2.0, c: "#ffff00", mass: 1.0 }
          , { i: 7, x: 30.0, y: 30.0, r: 5.0, xv: -0.05, yv: 2.5, c: "#00ffff", mass: 1.0 }
          ]
      setInterval
        25
        (do Canvas.clearRect ctx { x: 0.0, y: 0.0, w: width, h: height }
            Canvas.save ctx
            balls <- traverse readRef ballRefs
            traverse
              (\ref -> do
                ball <- readRef ref
                let ball' =
                      foldl
                        (\a0 b ->
                          let xdist = ((a0.x + a0.xv) - (b.x + b.xv))
                              ydist = ((a0.y + a0.yv) - (b.y + b.yv))
                              distSquared = xdist*xdist + ydist*ydist
                          in if a0.i == b.i
                                then a0
                                else
                                  let a =
                                        a0
                                        { xv = if a0.x+a0.xv < 0.0 || a0.x+a0.xv > width
                                               then -a0.xv
                                               else a0.xv
                                        , yv = if a0.y+a0.yv < 0.0 || a0.y+a0.yv > height
                                               then -a0.yv
                                               else a0.yv
                                        }
                                  in
                                  if distSquared <= (a.r + b.r) * (a.r + b.r)
                                     then let xvelocity = b.xv - a.xv
                                              yvelocity = b.yv - a.yv
                                              dotProduct = xdist*xvelocity + ydist*yvelocity
                                          in if dotProduct > 0.0
                                                then let collisionScale = dotProduct / distSquared
                                                         xcol = xdist * collisionScale
                                                         ycol = ydist * collisionScale
                                                         combinedMass = a.mass + b.mass
                                                         colWeightA = 2.0 * b.mass / combinedMass
                                                         colWeightB = 2.0 * a.mass / combinedMass
                                                     in a
                                                        { xv = a.xv + colWeightA * xcol
                                                        , yv = a.yv + colWeightA * ycol
                                                        }
                                                else a
                                     else a
                          )
                        ball
                        balls
                let ball'' = ball'
                      { x = ball'.x + ball'.xv
                      , y = ball'.y + ball'.yv
                      -- , xv = ball'.xv * 0.99
                      -- , yv = ball'.yv * 0.99
                      }
                writeRef ref ball''
                circle ctx ball'')
              ballRefs
            Canvas.restore ctx
            pure unit)
      pure unit

circle ctx c = do
  Canvas.beginPath ctx
  Canvas.setFillStyle c.c ctx
  Canvas.arc ctx { x: c.x, y: c.y, r: c.r, start: 0.0, end: Math.pi * 2.0 }
  Canvas.stroke ctx
  Canvas.fill ctx
  Canvas.closePath ctx
