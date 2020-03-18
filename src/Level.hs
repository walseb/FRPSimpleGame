module Level where

import Control.Lens
import Linear
import Types
import FRPEngine.Types

initialGame =
  GameState
    (CameraState 3)
  (PhysicalState
    (CollObj
    []
      (Object (V2 0 0) (V2 100 100) 0 SobjectSprite2))
    [(CollObj
     []
       (Object (V2 0 100) (V2 100 100) 0 SobjectSprite))])
