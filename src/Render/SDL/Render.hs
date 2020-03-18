module Render.SDL.Render where

import Control.Lens
import Linear
import FRPEngine.Render.SDL.Primitives
import qualified SDL as S
import FRPEngine.Types
import Types

render :: S.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer res (game@(GameState (CameraState zoomLevel) (PhysicalState player enemies)), exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 0 255
    S.clear renderer

    renderSpr (player ^. obj)
    sequence $ renderSpr . (^. obj) <$> enemies

    S.present renderer
    return exit
  where
    -- Static stuff center rot at top left
    renderObj' = renderObj (player ^. (obj . pos)) (flip getSprite res) (fromIntegral zoomLevel) renderer
    renderSpr = renderObj' True
    -- renderTerr = renderObj' False
    -- renderText' = renderText renderer (res ^. font)
    -- renderPt pos = renderObj' True (Object pos (V2 50 50) 0 SobjectSprite2)
