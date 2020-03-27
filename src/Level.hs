module Level where

import Data.Fixed
import FRPEngine.Types
import Linear
import Types

boxColl = [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]

moveBlock mod a = a + (99 * mod)

moveBlockUp = moveBlock 1

moveBlockDown = moveBlock (-1)

pureRandom :: (Real a) => a -> V2 a
pureRandom a = (V2 (3000 + a * 200) ((a ^ 5) `mod'` 501))

initialGame :: GameState
initialGame =
  GameState
    (CameraState 3)
    ( PhysicalState
        ( CollObj
            boxColl
            (Obj (V2 0 0) (V2 100 100) 0 SobjectSprite2 True)
        )
        ( [ -- Terrain
            ( CollObj
                [[]]
                (Obj (V2 (-10000) 549) (V2 50000000 5000) 0 SobjectSprite False)
            ),
            ( CollObj
                [[]]
                (Obj (V2 (-10000) (-5050)) (V2 50000000 5000) 0 SobjectSprite False)
            ),
            -- Enemies

            -- Wall top bottom
            ( CollObj
                boxColl
                (Obj (V2 1000 (-1)) (V2 100 100) 0 SobjectSprite True)
            ),
            ( CollObj
                boxColl
                (Obj (V2 1000 (moveBlockUp (-1))) (V2 100 100) 0 SobjectSprite True)
            ),
            ( CollObj
                boxColl
                (Obj (V2 1000 ((moveBlockUp . moveBlockUp) (-1))) (V2 100 100) 0 SobjectSprite True)
            ),
            -- Wall bottom top
            ( CollObj
                boxColl
                (Obj (V2 1500 501) (V2 100 100) 0 SobjectSprite True)
            ),
            ( CollObj
                boxColl
                (Obj (V2 1500 (moveBlockDown 501)) (V2 100 100) 0 SobjectSprite True)
            ),
            ( CollObj
                boxColl
                (Obj (V2 1500 ((moveBlockDown . moveBlockDown) 501)) (V2 100 100) 0 SobjectSprite True)
            ),
            -- Middle wall
            ( CollObj
                boxColl
                (Obj (V2 2000 250) (V2 100 100) 0 SobjectSprite True)
            ),
            ( CollObj
                boxColl
                (Obj (V2 2000 (moveBlockDown 250)) (V2 100 100) 0 SobjectSprite True)
            ),
            ( CollObj
                boxColl
                (Obj (V2 2000 (moveBlockUp 250)) (V2 100 100) 0 SobjectSprite True)
            ),
            -- Wall middle open
            ( CollObj
                boxColl
                (Obj (V2 2500 (-1)) (V2 100 100) 0 SobjectSprite True)
            ),
            ( CollObj
                boxColl
                (Obj (V2 2500 (moveBlockUp (-1))) (V2 100 100) 0 SobjectSprite True)
            ),
            ( CollObj
                boxColl
                (Obj (V2 2500 501) (V2 100 100) 0 SobjectSprite True)
            ),
            ( CollObj
                boxColl
                (Obj (V2 2500 (moveBlockDown 501)) (V2 100 100) 0 SobjectSprite True)
            )
          ]
            -- Semi-random level
            ++ ((\a -> CollObj boxColl (Obj (pureRandom a) (V2 100 100) 0 SobjectSprite True)) <$> [2 .. 13] ++ [15 .. 31] ++ [34 .. 49] ++ [51 .. 56] ++ [58 .. 99])
        )
    )
    True
