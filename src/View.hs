{-# LANGUAGE OverloadedStrings #-}

module View where

import Apecs
import Control.Lens ((^.))
import JSDOM
import JSDOM.Generated.AudioContext
import qualified JSDOM.Generated.CanvasRenderingContext2D as JCanvas
import JSDOM.Generated.Document
import JSDOM.Generated.HTMLCanvasElement
import JSDOM.Generated.HTMLCollection
import JSDOM.Generated.HTMLMediaElement (play)
import JSDOM.Types (HTMLAudioElement, JSContextRef, JSString, askJSM, fromJSValUnchecked, liftJSM, pFromJSVal, pToJSVal, runJSM, unElement)
import Language.Javascript.JSaddle
import Miso
import Miso.String
import ThreeVRM
import Types
import Pixi (newApp, loadTexture, createSprite, addStage, setSpriteSize)
import qualified Const

drawBackground :: JSContextRef -> System World ()
drawBackground c = do
  liftIO $ flip runJSM c $ do
    app <- newApp
    tex <- loadTexture "assets/img/background.png"
    sprite <- createSprite tex
    setSpriteSize sprite Const.width Const.height
    addStage app sprite
    return ()

drawEntity :: JSContextRef -> System World ()
drawEntity c = do
  liftIO $
    flip runJSM c $ do
      audioContext <- newAudioContext
      jsval <- getElementById "buttonSe"
      audioElement <- fromJSValUnchecked jsval :: JSM HTMLAudioElement
      play audioElement
  liftIO $ flip runJSM c $ do
    app <- newApp
    tex <- loadTexture "assets/img/background.png"
    sprite <- createSprite tex
    addStage app sprite
    return ()
