{-# LANGUAGE Arrows #-}

module Actor where

import Control.Lens
import Data.Coerce
import FRP.Yampa
import FRPEngine.Input.Interpreter (vectorizeMovement)
import FRPEngine.Input.Types
import FRPEngine.Types
import FRPEngine.YampaUtils.Types ()
import Linear

speed :: (Num a) => V2 a
speed = 500

forwardSpeed :: (RealFloat a) => a
forwardSpeed = 500

type MoveKeys a = V2 a

type InitPos a = V2 a

move :: (RealFloat a) => InitPos a -> SF (MoveKeys a) (V2 a, Event (V2 a))
move (V2 iPX iPY) = proc dir -> do
  posX <- integralFrom (V1 iPX) -< V1 forwardSpeed
  posY <- integralFrom (V1 iPY) -< V1 ((dir * speed) ^. _y)
  returnA -<
    let pos = coerce <$> V2 posX posY
     in (pos, restrict pos)
  where
    restrict (V2 x y)
      | y > 500 = Event (V2 x 500)
      | y < 0 = Event (V2 x 0)
      | otherwise = NoEvent

playerRun :: (RealFloat a) => Obj a _b -> SF InputState (Obj a _b)
playerRun initObj = proc input -> do
  pos' <- moveSwitch (initObj ^. pos) -< vectorizeMovement (input ^. movement)
  returnA -< pos .~ pos' $ initObj

moveSwitch :: (RealFloat a) => InitPos a -> SF (MoveKeys a) (V2 a)
moveSwitch initPos =
  switch
    (move initPos)
    moveSwitch
