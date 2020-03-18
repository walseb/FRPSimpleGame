{-# LANGUAGE Arrows #-}

module Actor where

import FRPEngine.YampaUtils.Types ()
import FRP.Yampa
import Linear
import FRPEngine.Types
import FRPEngine.Input.Interpreter (vectorizeMovement)
import FRPEngine.Input.Types
import Control.Lens

type MoveKeys a = V2 a
type InitPos a = V2 a

move :: (RealFloat a) => InitPos a -> SF (MoveKeys a) (V2 a)
move initPos = proc dir -> do
  hmm the player doesn't move
  pos <- integralFrom initPos -< dir
  returnA -< pos

playerRun :: (RealFloat a) => Object a _b -> SF (InputState) (Object a _b)
playerRun initObj = proc input -> do
  pos' <- move (initObj ^. pos) -< (vectorizeMovement (input ^. movement))
  returnA -< pos .~ pos' $ initObj
