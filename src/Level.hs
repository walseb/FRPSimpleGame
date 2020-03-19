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
    [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
      (Obj (V2 0 0) (V2 100 100) 0 SobjectSprite2 True))
    [(CollObj [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
       (Obj (V2 0 100) (V2 100 100) 0 SobjectSprite True))
    ])
