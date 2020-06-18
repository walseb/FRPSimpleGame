{-# LANGUAGE Arrows #-}

module Actor where

import Control.Lens
import Data.Coerce
import FRP.Yampa
import FRPEngine.Input.Types
import FRPEngine.Types
import FRPEngine.Yampa.Types ()
import Linear
import Input

speed :: (Num a) => V2 a
speed = 500

forwardSpeed :: (Number a) => a
forwardSpeed = 500

maxX :: (Number a) => a
maxX = 500
minX :: (Number a) => a
minX = 0

type MoveKeys a = V2 a
type InitPos a = V2 a

move :: (Number a) => InitPos a -> SF (MoveKeys a) (V2 a)
move initPos =
  switch
    (move' initPos)
    move
  where
    move' (V2 iPX iPY) = proc dir -> do
      posX <- integralFrom (V1 iPX) -< V1 forwardSpeed
      posY <- integralFrom (V1 iPY) -< V1 ((dir * speed) ^. _y)
      returnA -<
        let pos = coerce <$> V2 posX posY
         in (pos, restrict pos)
      where
        restrict (V2 x y)
          | y > maxX = Event (V2 x maxX)
          | y < minX = Event (V2 x minX)
          | otherwise = NoEvent

playerRun :: (Number a) => Obj a _b -> SF [Input] (Obj a _b)
playerRun initObj = proc input -> do
  pos' <- move (initObj ^. pos) -< moveKey input
  returnA -< pos .~ pos' $ initObj
