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
    isUndefined,
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
    (<#), ghcjsPure, isNull
  )
import Miso (consoleLogJSVal, div_, getElementById, id_)
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
  (args <# "antialias") True
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
  maybeBar <- app ! "stage" # "getChildByName" $ [name]
  b <- ghcjsPure $ isNull maybeBar
  return $
    if b
      then Nothing 
      else Just maybeBar

pixiCanvas = div_ [id_ . ms $ "pixiCanvas"] []

drawPlayerBar :: Float -> Float -> Float -> JSVal -> JSM ()
drawPlayerBar x y length bar = do
  (bar <# "x") (x - length / 2)
  (bar <# "y") y
  bar # "clear" $ ()
  bar # "beginFill" $ [0xAA1111 :: Float]
  bar # "drawRect" $ [0, 0, length, 10]
  bar # "endFill" $ ()
  return ()

type PixiBall = JSVal

createNamedGraphics :: String -> JSM JSVal
createNamedGraphics name = do
  g <- createGraphics
  (g <# "name") name
  return g

createGraphics = eval "new PIXI.Graphics();"

drawBall :: Float -> Float -> Float -> PixiBall -> JSM ()
drawBall x y radius ball = do
  (ball <# "x") x
  (ball <# "y") y
  ball # "clear" $ ()
  ball # "beginFill" $ [0xEE1111 :: Float]
  ball # "drawCircle" $ [0, 0, radius]
  ball # "endFill" $ ()
  return ()