module ThreeVRM where

import Control.Lens ((^.))
import JSDOM.Types (HTMLCanvasElement)
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
    new,
    obj,
    runJSaddle,
    val,
    valToJSON,
    valToText,
    (<#),
  )
import Miso (canvas_, consoleLog, consoleLogJSVal, height_, id_, width_, getElementById)
import Miso.String (ms)

threeCanvas =
  canvas_
    [ id_ $ ms "threeCanvas",
      width_ $ ms "400",
      height_ $ ms "300"
    ]
    []

getThreeCanvas :: JSM HTMLCanvasElement 
getThreeCanvas = do
  jsval <- getElementById $ ms "threeCanvas"
  fromJSValUnchecked jsval

type Scene = JSVal

newScene :: JSM Scene
-- newScene = new (jsg "THREE.Scene") ()
newScene = do
  scene <- eval "new THREE.Scene();"
  light <- new (jsg "THREE" ^. js "DirectionalLight") (val "#fff")
  light ^. js "position" . js3 "set" "-1" "1" "-1" . js0 "normalize"
  scene ^. js1 "add" light
  consoleLogJSVal scene
  return scene

addToScene scene elem = scene ^. js1 "add" elem

type Camera = JSVal

newCamera :: JSM Camera
newCamera = do
  camera <- eval "new THREE.PerspectiveCamera(45, 400/300, 0.1, 1000)"
  camera ^. js "position" . js3 "set" "0" "0.8" "-2"
  camera ^. js "rotation" . js3 "set" "0" (jsg "Math" ^. js "PI") "0"
  return camera

type Renderer = JSVal

newRenderer :: HTMLCanvasElement -> JSM Renderer
newRenderer canvas = do
  args <- obj
  (args <# "canvas") canvas
  (args <# "antialias") True
  renderer <- new (jsg "THREE" ^. js "WebGLRenderer") (val args)
  renderer ^. js1 "setPixelRatio" (eval "window.devicePixelRatio")
  renderer ^. js2 "setSize" "400" "300"
  renderer ^. js2 "setClearColor" "#7fbfff" "0.1"
  return renderer

render :: Renderer -> Scene -> Camera -> JSM ()
render renderer scene camera = do
  renderer ^. js2 "render" scene camera
  return ()

newGridHelper = eval "new THREE.GridHelper(10, 10)"

type Loader = JSVal

newLoader :: JSM Loader
newLoader = new (jsg "THREE" ^. js "GLTFLoader") ()

-- loadVRM :: Loader -> Scene -> JSM ()
loadVRM loader scene renderer camera = do
  _ <-
    loader
      ^. js4
        "load"
        "assets/vrm/256fes_2022.vrm"
        ( fun $ \_ _ [gltf] -> do
            consoleLog $ ms "gltf load"
            jsg "THREE" ^. js "VRM" . js1 "from" gltf
              . js1
                "then"
                ( fun $ \_ _ [vrm] -> do
                    scene ^. js1 "add" (vrm ^. js "scene")
                    consoleLogJSVal vrm
                    render renderer scene camera
                )
            return ()
        )
        ( fun $ \_ _ [progress] -> do
            consoleLogJSVal progress
            return ()
        )
        ( fun $ \_ _ [error] -> do
            consoleLogJSVal error
        )
  return ()