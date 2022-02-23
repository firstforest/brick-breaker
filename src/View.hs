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

drawEntity :: JSContextRef -> System World ()
drawEntity c = do
  liftIO $
    flip runJSM c $ do
      c <- getThreeCanvas
      scene <- newScene
      camera <- newCamera
      renderer <- newRenderer c
      loader <- newLoader
      grid <- newGridHelper
      addToScene scene grid
      loadVRM loader scene renderer camera
      render renderer scene camera
      return ()
  liftIO $
    flip runJSM c $ do
      audioContext <- newAudioContext
      jsval <- getElementById "buttonSe"
      audioElement <- fromJSValUnchecked jsval :: JSM HTMLAudioElement
      play audioElement

  cmapM_ $ \(Position p, Entity e) -> liftIO $ do
    flip runJSM c $ do
      consoleLog $ ms . show $ (e, p)

      canvasContext <- getCanvasContext
      let c = convertElem canvasContext
      JCanvas.clearRect c 0 0 300 300
      JCanvas.setFillStyle c ("blue" :: String)
      f <- JCanvas.getFont c
      consoleLog . ms $ (f :: String)
      JCanvas.fillText c (ms ("test" :: String)) 20 20 (Just 30)

getCanvas = do
  jsval <- getElementById "canvas"
  fromJSValUnchecked jsval

getCanvasContext = do
  canvas <- getCanvas
  getContextUnsafe canvas ("2d" :: String) ([] :: [JSString])

convertElem = pFromJSVal . pToJSVal