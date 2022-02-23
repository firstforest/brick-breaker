module Pixi where

import Control.Lens ((^.))
import Language.Javascript.JSaddle
  ( FromJSVal (fromJSValUnchecked),
    JSM,
    JSVal,
    ToJSVal (toJSVal),
    eval,
    freeFunction,
    fun,
    js,
    js0,
    js1,
    js2,
    js3,
    js4,
    jsg,
    jss,
    new,
    obj,
    runJSaddle,
    val,
    valToJSON,
    valToText,
    (<#), (!), (#)
  )
import Miso (div_, getElementById, id_)
import Miso.String (ms)

canvasWidth :: Float
canvasWidth = 640

canvasHeight :: Float
canvasHeight = 480

type Application = JSVal

newApp :: JSM JSVal
newApp = do
  args <- obj
  (args <# "width") canvasWidth
  (args <# "height") canvasHeight
  (args <# "backgroundColor") "#0x1099bb"
  app <- new (pixi ! "Application") (val args)
  div <- getElementById . ms $ "pixiCanvas"
  div # "appendChild" $ (app ^. js "view")
  return app

pixi = jsg "PIXI"

loadTexture :: String -> JSM JSVal
loadTexture url = new (pixi ! "Texture" ! "from") (val url)

createSprite :: JSVal -> JSM JSVal
createSprite tex = do
  sprite <- new (pixi ! "Sprite") tex
  sprite ! "anchor" # "set" $ [0.5::Float]
  sprite ! "scale" <# "x" $ (0.1 :: Float)
  sprite ! "scale" <# "y" $ (0.1 :: Float)
  sprite <# "x" $ (320::Float)
  sprite <# "y" $ (240::Float)
  return sprite

addStage app = app ! "stage" # "addChild"

pixiCanvas = div_ [id_ . ms $ "pixiCanvas"] []