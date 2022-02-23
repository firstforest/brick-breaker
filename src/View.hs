{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module View where

import Apecs
import qualified Const
import Control.Lens ((^.))
import Game
import JSDOM
import Language.Javascript.JSaddle
import Miso
import Miso.String
import Pixi (addStage, createSprite, loadTexture, newApp, setSpriteSize, findChild, createBar, drawPlayerBar)
import Types
import Linear (V2(V2))

drawBackground :: Context -> System' ()
drawBackground Context{..} = do
  liftToSystem jsContext  $ do
    tex <- loadTexture "assets/img/background.png"
    sprite <- createSprite tex
    setSpriteSize sprite Const.width Const.height
    addStage pixiApp sprite
    return ()

liftToSystem c = liftIO . flip runJSM c

drawEntity :: JSContextRef -> System' ()
drawEntity c = do
  liftToSystem c $ do
    app <- newApp
    tex <- loadTexture "assets/img/background.png"
    sprite <- createSprite tex
    addStage app sprite
    return ()

drawBar :: Context -> System' ()
drawBar Context{..} = do
  cmapM_ $ \(Bar {length}, Position (V2 x y)) -> do
    liftToSystem jsContext $ do
      maybeChild <- findChild "bar" pixiApp
      bar <- case maybeChild of
        Just bar -> return bar
        Nothing -> do
          bar <- createBar 
          addStage pixiApp bar
          consoleLog "createBar"
          return bar
      drawPlayerBar x y length bar 
      return ()

