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
  new (pixi ! "Application") (val args)

initializeApp :: Application -> JSM ()
initializeApp app = do
  div <- getElementById . ms $ "pixiCanvas"
  div # "appendChild" $ [app ^. js "view"]
  return ()

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

findChild :: String -> Application -> JSM (Maybe JSVal)
findChild name app = do
  return Nothing

pixiCanvas = div_ [id_ . ms $ "pixiCanvas"] []

createBar = do
  eval "new PIXI.Graphics().beginFill(0x333333).drawRect(0,0,50,10).endFill();"

drawPlayerBar :: Float -> Float -> Float -> JSVal -> JSM ()
drawPlayerBar x y length bar = do
  (bar <# "x") (x - length/ 2)
  (bar <# "y") y
  bar # "clear" $ ()
  bar # "beginFill" $ [0xAA1111 :: Float]
  bar # "drawRect" $ [0, 0, length, 10]
  bar # "endFill" $ ()
  return ()