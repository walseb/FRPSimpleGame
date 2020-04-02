module Level where

import Control.Applicative
import Control.Monad
import Data.Fixed
import Data.Maybe
import FRPEngine.Types
import Linear
import Types

boxColl :: (RealFloat a) => [[V2 a]]
boxColl = [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]

moveBlock mod a = a + (99 * mod)

moveBlockUp = moveBlock 1

moveBlockDown = moveBlock (-1)

pureRandom :: (Real a) => a -> V2 a
pureRandom a = (V2 (3000 + a * 200) ((a ^ 5) `mod'` 501))

type Slice = [Bool]

type Lvl = [Slice]

build :: (RealFloat a) => Lvl -> [CollObj a SpriteSelect]
build lvl = join $ zipWith (flip buildSlice) lvl y
  where
    -- 99 is here to fix the gap problem with integer rendering positions
    y = realToFrac <$> [0,99 ..]

buildSlice :: (RealFloat a) => a -> Slice -> [CollObj a SpriteSelect]
buildSlice x things@(a : b : c : d) =
  catMaybes $ buildSingle <$> (zip pos things)
  where
    y = realToFrac <$> [-1,99 .. 506]
    pos = liftA2 V2 [x] y

buildSingle :: (RealFloat a) => (V2 a, Bool) -> Maybe (CollObj a SpriteSelect)
buildSingle ((V2 x y), True) =
  Just
    ( CollObj
        boxColl
        (Obj (V2 x y) (V2 100 100) 0 SobjectSprite True)
    )
buildSingle ((V2 x y), False) =
  Nothing

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
                (Obj (V2 (-10000) 547) (V2 50000000 5000) 0 SobjectSprite False)
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
            ++ (build
               [
                 -- Ok so to transform this quickly just turn all to False, then search and replace False with True. Then you just press y or n until you have the layout you want
                 [True, False, False, True, False, True],
                 [True, False, False, True, False, True],
                 [True, False, False, True, False, True],
                 [True, False, False, True, False, True],
                 [True, False, False, True, False, True],
                 [True, False, False, True, False, True],
                 [True, False, False, True, False, True]
               ]
              )
          -- Semi-random level
          -- ++ ((\a -> CollObj boxColl (Obj (pureRandom a) (V2 100 100) 0 SobjectSprite True)) <$> [2 .. 13] ++ [15 .. 31] ++ [34 .. 49] ++ [51 .. 56] ++ [58 .. 91])
        )
    )
    True
