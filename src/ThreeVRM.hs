{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant ^." #-}

module ThreeVRM where

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
    new,
    obj,
    runJSaddle,
    val,
    valToJSON,
    valToText,
    (<#),
  )
import Miso (consoleLog, consoleLogJSVal)
import Miso.String (ms)

type Scene = JSVal

newScene :: JSM Scene
-- newScene = new (jsg "THREE.Scene") ()
newScene = do
  scene <- eval "new THREE.Scene();"
  light <- new (jsg "THREE" ^. js "DirectionalLight") (val "#fff")
  light ^. js "position" ^. js3 "set" "-1" "1" "-1" ^. js0 "normalize"
  scene ^. js1 "add" light
  consoleLogJSVal scene
  return scene

newCamera = do
  camera <- eval "new THREE.PerspectiveCamera(45, 400/300, 0.1, 1000)"
  camera ^. js "position" ^. js3 "set" "0" "0.8" "-2"
  camera ^. js "rotation" ^. js3 "set" "0" (jsg "Math" ^. js "PI") "0"
  return camera

newRenderer canvas = do
  args <- obj
  (args <# "canvas") canvas
  renderer <- new (jsg "THREE" ^. js "WebGLRenderer") (val args)
  renderer ^. js1 "setPixelRatio" (eval "window.devicePixelRatio")
  renderer ^. js2 "setSize" "400" "300"
  renderer ^. js2 "setClearColor" "#7fbfff" "0.1"
  return renderer

render renderer scene camera = do
  renderer ^. js2 "render" scene camera
  consoleLogJSVal scene

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
            jsg "THREE" ^. js "VRM" ^. js1 "from" gltf
              ^. js1
                "then"
                ( fun $ \_ _ [vrm] -> do
                    scene ^. js1 "add" (vrm ^. js "scene")
                    consoleLogJSVal vrm
                    consoleLogJSVal scene
                    render renderer scene camera
                )
            return ()
        )
        (fun $ \_ _ [progress] -> return ())
        ( fun $ \_ _ [error] -> do
            consoleLogJSVal error
        )
  return ()