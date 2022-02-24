{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module View
  ( drawEntities,
    drawBackground,
    initializeView
  )
where

import Apecs
import qualified Const
import Control.Lens ((^.))
import Game
import JSDOM
import Language.Javascript.JSaddle
import Linear (V2 (V2))
import Miso
import Miso.String
import Pixi (addStage, createNamedGraphics, createSprite, drawPlayerBar, findChild, loadTexture, newApp, setSpriteSize)
import qualified Pixi
import Types

initializeView :: Context -> System' ()
initializeView Context {..} = do
  liftToSystem jsContext $ do
    createGraphics pixiApp "bar"
    return ()

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
  drawBlock c

createGraphics pixiApp name = do
  bar <- createNamedGraphics name
  addStage pixiApp bar
  consoleLog . ms $ "createGraphics" <> name
  return bar

drawBar :: Context -> System' ()
drawBar Context {..} = do
  cmapM_ $ \(Bar {length}, Position (V2 x y)) -> do
    liftToSystem jsContext $ do
      maybeChild <- findChild "bar" pixiApp
      bar <- case maybeChild of
        Just bar -> return bar
        Nothing -> createGraphics pixiApp "bar"
      drawPlayerBar x y length bar
      return ()

drawBall :: Context -> System' ()
drawBall Context {..} = do
  cmapM $ \(Ball {id, radius}, Position (V2 x y)) -> do
    liftToSystem jsContext $ do
      maybeBall <- findChild id pixiApp
      ball <- case maybeBall of
        Just ball -> return ball
        Nothing -> createGraphics pixiApp id
      Pixi.drawBall x y radius ball
      return ()

drawBlock :: Context -> System' ()
drawBlock Context {..} = do
  cmapM $ \(Block {blockId}, Position (V2 x y)) -> do
    liftToSystem jsContext $ do
      maybeBlock <- findChild blockId pixiApp
      block <- case maybeBlock of
        Just b -> pure b
        Nothing -> createGraphics pixiApp blockId
      Pixi.drawBlock x y block