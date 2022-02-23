{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module View (
  drawEntities,
  drawBackground
) where

import Apecs
import qualified Const
import Control.Lens ((^.))
import Game
import JSDOM
import Language.Javascript.JSaddle
import Linear (V2 (V2))
import Miso
import Miso.String
import qualified Pixi
import Pixi (addStage, createSprite, drawPlayerBar, findChild, loadTexture, newApp, setSpriteSize, createNamedGraphics)
import Types

drawBackground :: Context -> System' ()
drawBackground Context {..} = do
  liftToSystem jsContext $ do
    tex <- loadTexture "assets/img/background.png"
    sprite <- createSprite tex
    setSpriteSize sprite Const.width Const.height
    addStage pixiApp sprite
    return ()

liftToSystem c = liftIO . flip runJSM c

drawEntities :: Context -> System' ()
drawEntities c = do
  drawBar c
  drawBall c

drawBar :: Context -> System' ()
drawBar Context {..} = do
  cmapM_ $ \(Bar {length}, Position (V2 x y)) -> do
    liftToSystem jsContext $ do
      maybeChild <- findChild "bar" pixiApp
      bar <- case maybeChild of
        Just bar -> return bar
        Nothing -> do
          bar <- createNamedGraphics "bar"
          addStage pixiApp bar
          consoleLog "createBar"
          return bar
      drawPlayerBar x y length bar
      return ()

drawBall :: Context -> System' ()
drawBall Context {..} = do
  cmapM_ $ \(Ball {radius}, Position (V2 x y)) -> do
    liftToSystem jsContext $ do
      maybeBall <- findChild "ball" pixiApp
      ball <- case maybeBall of
        Just ball -> return ball
        Nothing -> do
          ball <- createNamedGraphics "ball"
          addStage pixiApp ball
          consoleLog "createBall"
          return ball
      Pixi.drawBall x y radius ball
      return ()