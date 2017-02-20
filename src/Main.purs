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

-- NOTE: need a way for one signal of a sensor to reach an actor
-- elsewhere in the organism.

newtype Organism = Organism
  { cells :: Array (Ref Cell)
  }

newtype Cell = Cell
  { x :: Number
  , y :: Number
  , neighbors :: Array (Ref Cell)
  , r :: Number
  }

newtype Input = Input
  { timeTick :: Int
  , age :: Int
  , xvelocity :: Number
  , yvelocity :: Number
  , local_energy :: Number
  , total_energy :: Number
  , rotation :: Number
  , light :: Array Number
  , sound :: Number
  , local_nonconnected_touch :: Boolean
  , foreign_touch :: Number
  }

data Sensor
  = Tick NumberCondition
  | Age NumberCondition
  | XVelocityBelow NumberCondition
  | XVelocityAbove NumberCondition
  | YVelocityBelow NumberCondition
  | YVelocityAbove NumberCondition
  | LocalEnergy NumberCondition
  | Rotation NumberCondition
  | Light ArrayCondition
  | Sound ArrayCondition
  | LocalTouch Boolean
  | ForeignTouch Boolean
  | And Sensor Sensor
  | Or Sensor Sensor

data ArrayCondition
  = All NumberCondition
  | None NumberCondition
  | Conditions (Array NumberCondition)

data NumberCondition
  = NumberAbove Number
  | NumberBelow Number
  | NumberInRange Number Number

data Action
  = Articulate Number
  | Propel Number
  | Absorb
  | Reproduce
  | ShareEnergy Number
  | MakeSound { strength :: Number, duration :: Int }
  | MakeLight { strength :: Number, duration :: Int }
  | Grow Number
  | Disconnect
  | Connect

data Strength

main = do
  mcanvas <- Canvas.getCanvasElementById "canvas"
  case mcanvas of
    Nothing -> pure unit
    Just canvas -> do
      width <- Canvas.getCanvasWidth canvas
      height <- Canvas.getCanvasWidth canvas
      log ("Width " <> show width <> ", height " <> show height)
      ctx <- Canvas.getContext2D canvas
      setInterval
        50
        (do Canvas.clearRect ctx { x: 0.0, y: 0.0, w: width, h: height }
            Canvas.save ctx
            Canvas.restore ctx
            pure unit)
      pure unit

circle ctx pos r = do
  Canvas.beginPath ctx
  Canvas.arc ctx { x: pos.x, y: pos.y, r: r, start: 0.0, end: Math.pi * 2.0 }
  Canvas.stroke ctx
  Canvas.closePath ctx
