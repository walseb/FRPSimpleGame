{-# LANGUAGE Arrows #-}

module Actor where

import FRPEngine.YampaUtils.Types ()
import FRP.Yampa
import Linear
import FRPEngine.Types
import FRPEngine.Input.Interpreter (vectorizeMovement)
import FRPEngine.Input.Types
import Control.Lens

speed :: (Num a) => V2 a
speed = 500

type MoveKeys a = V2 a
type InitPos a = V2 a

move :: (RealFloat a) => InitPos a -> SF (MoveKeys a) (V2 a)
move initPos = proc dir -> do
  pos <- integralFrom initPos -< (V2 0 ((dir * speed) ^. _y))
  returnA -< pos

playerRun :: (RealFloat a) => Obj a _b -> SF (InputState) (Obj a _b)
playerRun initObj = proc input -> do
  pos' <- move (initObj ^. pos) -< vectorizeMovement (input ^. movement)
  returnA -< pos .~ pos' $ initObj
