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

import qualified Const
import Control.Monad (forM_, replicateM)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Float (double2Float)
import Game
import JSDOM (currentWindowUnchecked)
import JSDOM.Custom.Window (requestAnimationFrame_)
import JSDOM.Generated.HTMLMediaElement (play)
import JSDOM.Types (Callback (Callback), HTMLAudioElement, JSContextRef, JSString, RequestAnimationFrameCallback (RequestAnimationFrameCallback), askJSM, fromJSValUnchecked, liftJSM, pFromJSVal, pToJSVal, runJSM, unElement)
import Language.Javascript.JSaddle (MakeObject (makeObject), function, val)
import Language.Javascript.JSaddle.Object (Function (Function))
import Linear (V2 (..))
import Miso
import Miso.String
import Pixi (initializeApp, newApp, pixiCanvas)
import ThreeVRM (threeCanvas)
import Types
import View

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = NoOp
  | Initialize
  | Tick Float
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
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

registerAnimationFrame :: (Float -> JSM ()) -> JSM ()
registerAnimationFrame f = do
  n <- double2Float <$> now
  ref <- liftIO $ newIORef n
  window <- currentWindowUnchecked
  f <- function $ \a b c -> do
    prev <- liftIO $ readIORef ref
    n <- double2Float <$> now
    liftIO $ writeIORef ref n
    f $ n - prev
    d <- makeObject a
    requestAnimationFrame_ window (RequestAnimationFrameCallback (Callback (Function d)))
  requestAnimationFrame_ window (RequestAnimationFrameCallback (Callback f))

animationFrameSub :: (Float -> Action) -> Sub Action
animationFrameSub act sink = do
  registerAnimationFrame $ \x -> do
    liftIO $ sink $ act x

-- | Entry point for a miso application
main :: IO ()
main = do
  world <- initWorld
  runApp $ do
    c <- askJSM
    pixiApp <- newApp
    -- registerAnimationFrame $ \x -> consoleLog $ ms x
    let update = updateModel (Context c pixiApp world)
    startApp
      App
        { update = update,
          ..
        }
  where
    initialAction = Initialize
    model = 0
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs =
      [ mouseSub $ \(x, y) -> Move $ fromIntegral x - offset,
        animationFrameSub $ \x -> Tick (x / 1000)
      ] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = Off -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Context -> Action -> Model -> Effect Action Model
updateModel c@Context {..} Initialize m =
  m <# do
    liftIO $ runSystem initialize world
    initializeApp pixiApp
    liftIO $ runSystem (drawBackground c) world
    -- liftIO $
    --   flip runJSM c $ do
    --     jsval <- getElementById "bgm"
    --     audioElement <- fromJSValUnchecked jsval :: JSM HTMLAudioElement
    --     play audioElement
    return NoOp
updateModel Context {..} NoOp m = noEff m
updateModel c@Context {..} (Tick dt) m =
  m <# do
    liftIO $ runSystem (step dt) world
    liftIO $ runSystem (drawEntities c) world
    return NoOp
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
      audio_ [src_ "assets/bgm/chess.mp3", id_ "bgm"] []
    ]
