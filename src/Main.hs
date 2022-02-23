{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Apecs
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import qualified Network.Wai as Wai
import           Network.WebSockets
import qualified Network.Wai.Application.Static as Static
import qualified DevServer
#endif

import Control.Monad (replicateM)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import JSDOM.Generated.HTMLMediaElement (play)
import JSDOM.Types (HTMLAudioElement, JSContextRef, JSString, askJSM, fromJSValUnchecked, liftJSM, pFromJSVal, pToJSVal, runJSM, unElement)
import Language.Javascript.JSaddle (val)
import Linear (V2 (..))
import Miso
import Miso.String
import ThreeVRM (threeCanvas)
import Types
import View
import Pixi (pixiCanvas)

game :: System World ()
game = do
  newEntity (Position 0, Velocity 1)
  newEntity (Position 2, Velocity 1)
  newEntity (Position 1, Velocity 2, Flying)

  cmap $ \(Position p, Velocity v) -> Position (v + p)
  cmap $ \(Velocity v, _ :: Not Flying) -> Velocity (v - V2 0 1)

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

libraries =
  [ "assets/js/three.js",
    "assets/js/GLTFLoader.js",
    "assets/js/three-vrm.js",
    "assets/js/pixi.js"
  ]

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f = do
  jsList <- mapM B.readFile libraries
  let js = Prelude.foldr (<>) "" jsList
  DevServer.debugOr 8080 (f >> syncPoint) js DevServer.fileServer
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = do
  world <- initWorld
  runApp $ do
    c <- askJSM
    let update = updateModel (c, world)
    startApp
      App
        { update = update,
          ..
        }
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model = 0
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [arrowsSub (const SubtractOne)] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = Off -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: (JSContextRef, World) -> Action -> Model -> Effect Action Model
updateModel (_, w) AddOne m = noEff (m + 1)
updateModel (c, w) SubtractOne m = do
  m <# do
    liftIO (runSystem (drawEntity c) w)
    pure NoOp
  noEff (m - 1)
updateModel (_, w) NoOp m = noEff m
updateModel (c, w) SayHelloWorld m =
  m <# do
    liftIO $
      flip runJSM c $ do
        jsval <- getElementById "bgm"
        audioElement <- fromJSValUnchecked jsval :: JSM HTMLAudioElement
        play audioElement
    liftIO (runSystem game w)
    consoleLog "HelloWorld"
    liftIO (putStrLn "Hello World") >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x =
  div_
    []
    [ button_ [onClick AddOne] [text "+"],
      text (ms x),
      button_ [onClick SubtractOne] [text "-"],
      threeCanvas,
      pixiCanvas,
      canvas_
        [ id_ "canvas",
          width_ "400",
          height_ "300"
        ]
        [],
      audio_ [src_ "assets/se/button.mp3", id_ "buttonSe"] [],
      audio_ [src_ "assets/bgm/chess.mp3", id_ "bgm"] []
    ]
