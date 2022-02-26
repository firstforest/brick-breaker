{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

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

import qualified Const
import Control.Monad (forM_, replicateM, when)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Float (double2Float)
import Game
import JSDOM (currentWindowUnchecked)
import JSDOM.Custom.Window (requestAnimationFrame_)
import JSDOM.Generated.HTMLMediaElement (play)
import JSDOM.Types (Callback (Callback), HTMLAudioElement, JSContextRef, JSString, RequestAnimationFrameCallback (RequestAnimationFrameCallback), askJSM, fromJSValUnchecked, liftJSM, pFromJSVal, pToJSVal, runJSM, unElement)
import Language.Javascript.JSaddle (MakeObject (makeObject), function, val, fromJSVal)
import Language.Javascript.JSaddle.Object (Function (Function))
import Linear (V2 (..))
import Miso
import Miso.String
import Pixi (initializeApp, newApp, pixiCanvas)
import ThreeVRM (threeCanvas)
import Types
import View

-- | Type synonym for an application model
data Model = Model
  { isInitialized :: Bool,
    time :: Float
  }
  deriving (Show, Eq)

-- | Sum type for application events
data Action
  = NoOp
  | Initialize
  | Tick Float
  | RequestAnimationFrame Float
  | Move Float
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
requestTick :: Float -> Sink Action -> JSM ()
requestTick t sink = liftIO . sink $ Tick t
#else
runApp :: IO () -> IO ()
runApp app = app
requestTick :: Float -> Sink Action -> JSM ()
requestTick t sink = do
  window <- currentWindowUnchecked
  callback <- function \_ _ _ -> do
    liftIO . sink $ Tick t
  requestAnimationFrame_ window $ RequestAnimationFrameCallback $ Callback callback
#endif

-- | Entry point for a miso application
main :: IO ()
main = do
  world <- initWorld
  runApp $ do
    c <- askJSM
    pixiApp <- newApp
    time <- double2Float <$> now
    let update = updateModel (Context c pixiApp world)
    startApp
      App
        { model = Model False time ,
          update = update,
          ..
        }
  where
    initialAction = Initialize
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs =
      [ mouseSub $ \(x, y) -> Move $ fromIntegral x - offset
      ]
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = Off -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Context -> Action -> Model -> Effect Action Model
updateModel c@Context {..} Initialize m =
  m {isInitialized = True} <# do
    liftIO $ runSystem initialize world
    consoleLog "world initialized"
    initializeApp pixiApp
    liftIO $ runSystem (drawBackground c) world
    liftIO $ runSystem (initializeView c) world
    consoleLog "pixi initialized"
    time <- double2Float <$> now
    return $ Tick time
updateModel Context {..} NoOp m = noEff m
updateModel c@Context {..} (Tick time) m =
  m <# do
    n <- double2Float <$> now
    let dt = (n - time) / 1000
    consoleLog . ms $ dt
    prev <- double2Float <$> now
    when (isInitialized m) $ do
      liftIO $ runSystem (step dt) world
      liftIO $ runSystem (drawEntities c) world
    return $ RequestAnimationFrame n
updateModel c (RequestAnimationFrame t) m = effectSub m $ \sink -> do
  requestTick t sink
updateModel Context {..} (Move x) m =
  m <# do
    liftIO $ runSystem (moveBar x) world
    return NoOp

offset = (650 - Const.width) / 2

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x =
  div_
    [style_ $ ("padding" =: "12px" <> "background" =: "black" <> "display" =: "flex" <> "width" =: "650px" <> "flexDirection" =: "column" <> "alignItems" =: "center")]
    [ pixiCanvas,
      audio_ [src_ "assets/se/button.mp3", id_ "buttonSe"] [],
      audio_ [src_ "assets/bgm/chess.mp3", autoplay_ True, id_ "bgm"] []
    ]
