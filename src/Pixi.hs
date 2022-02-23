module Pixi where

import qualified Const
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
    (!),
    (#),
    (<#),
  )
import Miso (div_, getElementById, id_)
import Miso.String (ms)

canvasWidth :: Float
canvasWidth = Const.width

canvasHeight :: Float
canvasHeight = Const.height

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

type Sprite = JSVal

createSprite :: JSVal -> JSM Sprite
createSprite tex = do
  new (pixi ! "Sprite") tex

setSpriteSize :: Sprite -> Float -> Float -> JSM ()
setSpriteSize sprite w h = do
  sprite <# "width" $ w
  sprite <# "height" $ h

addStage app = app ! "stage" # "addChild"

pixiCanvas = div_ [id_ . ms $ "pixiCanvas"] []