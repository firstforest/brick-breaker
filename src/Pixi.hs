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
    ghcjsPure,
    isNull,
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
    (<#), toJSString, fromJSVal, JSValue, valToStr
  )
import Miso (consoleLogJSVal, div_, getElementById, id_, consoleLog)
import Miso.String (ms)
import Control.Monad
import JSDOM.Types (fromJSArrayUnchecked)
import JSDOM.Types

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

addStage :: Application -> JSVal -> JSM ()
addStage app x = do
  app ! "stage" # "addChild" $ [x]
  return ()

addChild :: JSVal -> JSVal -> JSM ()
addChild container x = do
  container # "addChild" $ [x]
  return ()

createContainer :: String -> JSM JSVal
createContainer name = do
  c <- eval "new PIXI.Container()"
  (c <# "name") name
  return c

findChild :: String -> Application -> JSM (Maybe JSVal)
findChild name app = do
  contaier <- app ! "stage"
  findChildFromContainer name contaier

findChildFromContainer name contaier = do
  maybeBar <- contaier # "getChildByName" $ [name]
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

drawBlock :: Float -> Float -> JSVal -> JSM ()
drawBlock x y block = do
  (block <# "x") (x - 30)
  (block <# "y") (y - 20)
  block # "clear" $ ()
  block # "beginFill" $ [0x11EE11 :: Float]
  block # "drawRect" $ ([1, 1, 59, 39] :: [Float])
  block # "endFill" $ ()
  return ()

clearBlocks :: Application -> JSM ()
clearBlocks app = do
  bs <- getAllBlocks app
  forM_ bs $ \b -> b # "clear" $ ()

getAllBlocks :: Application -> JSM [JSVal]
getAllBlocks app = do
  bc <- findChild "blockContainer" app
  case bc of
    Just b -> getChildren b
    Nothing -> do
      consoleLog .ms $ "error"
      return []

getChildren :: JSVal -> JSM [JSVal]
getChildren contaier = do
  children <- contaier ! "children"
  fromJSArrayUnchecked children

getBlockContainer app = do
  Just bc <- findChild "blockContainer" app
  return bc


deleteRemovedBlock :: Application -> [String] -> JSM ()
deleteRemovedBlock app ids = do
  bc <- getBlockContainer app
  blocks <- getChildren bc
  let remainIds = map toJSString ids
  forM_ blocks $ \block -> do
    n <- block ! "name" >>= valToStr
    when (n `notElem` remainIds) $ do
      bc # "removeChild" $ block
      return ()

