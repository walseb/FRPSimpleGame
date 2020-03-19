{-# LANGUAGE Arrows #-}

module Main
  ( main,
  )
where

import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import FRP.Yampa
import FRPEngine.Input.Types as I
import Level
import Render.SDL.Render
import qualified SDL as S
import qualified SDL.Font as F
import SDL.Image as SI
import Types
import FRPEngine.Init
import FRPEngine.Input.Input
import Actor
import FRPEngine.Types

runPhysical :: PhysicalState -> SF InputState PhysicalState
runPhysical (PhysicalState (CollObj iPC iP) iE) =
  proc input -> do
    p <- playerRun iP -< input
    returnA -< PhysicalState (CollObj iPC p) iE

run :: GameState -> SF InputState GameState
run (GameState (CameraState iZ) p) =
  proc input -> do
    physical <- runPhysical p -< input
    returnA -<
      ( GameState
          (CameraState iZ)
          physical
      )

update :: GameState -> SF (Event [S.Event]) (GameState, Bool)
update origGameState = proc events -> do
  newInputState <- accumHoldBy inputStateUpdate defaultKeybinds -< events
  gameState <- run origGameState -< newInputState
  returnA -<
    ( gameState,
      (fromJust (newInputState ^. I.quit ^? pressed))
    )

getResources :: (MonadIO m) => S.Renderer -> m Resources
getResources renderer =
  -- Init fonts
  Resources
    <$> (F.initialize >> F.load fontPath 12)
    <*> load spritePath renderer
    <*> load spritePath2 renderer
  where
    load :: (MonadIO m) => FilePath -> S.Renderer -> m S.Texture
    load path rend = SI.loadTexture rend path
    fontPath = "data/fonts/OpenSans-Regular.ttf"
    spritePath = "data/testSprite.png"
    spritePath2 = "data/testSprite2.png"

main =
  runSDL
    "FRP Lunar Lander"
    getResources
    (\renderer senseInput resources -> reactimate (return NoEvent) senseInput (\_ -> render renderer resources) (update initialGame))
