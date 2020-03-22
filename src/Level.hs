module Level where

import FRPEngine.Types
import Linear
import Types

initialGame :: GameState
initialGame =
  GameState
    (CameraState 3)
    ( PhysicalState
        ( CollObj
            [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
            (Obj (V2 0 0) (V2 100 100) 0 SobjectSprite2 True)
        )
        [ ( CollObj
              [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
              (Obj (V2 0 400) (V2 100 100) 0 SobjectSprite True)
          ),
          ( CollObj
              [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
              (Obj (V2 500 425) (V2 100 100) 0 SobjectSprite True)
          ),
          ( CollObj
              [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
              (Obj (V2 1000 523) (V2 100 100) 0 SobjectSprite True)
          ),
          ( CollObj
              [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
              (Obj (V2 2000 223) (V2 100 100) 0 SobjectSprite True)
          ),
          ( CollObj
              [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
              (Obj (V2 3000 509) (V2 100 100) 0 SobjectSprite True)
          ),
          ( CollObj
              [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
              (Obj (V2 2500 32) (V2 100 100) 0 SobjectSprite True)
          ),
          ( CollObj
              [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
              (Obj (V2 4000 235) (V2 100 100) 0 SobjectSprite True)
          ),
          ( CollObj
              [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
              (Obj (V2 1324 333) (V2 100 100) 0 SobjectSprite True)
          ),( CollObj
              [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]
              (Obj (V2 3843 400) (V2 100 100) 0 SobjectSprite True)

            )
        ]
    ) True
