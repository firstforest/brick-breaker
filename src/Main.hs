-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import           Miso
import           Miso.String

import Apecs
import Linear (V2 (..))
import JSDOM.Types (unElement,fromJSValUnchecked, pFromJSVal, pToJSVal, JSString, liftJSM, askJSM, runJSM, JSContextRef )

-- | JSAddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import           Control.Monad.IO.Class

import Types
import View

game :: System World ()
game = do
  newEntity (Position 0, Velocity 1)
  newEntity (Position 2, Velocity 1)
  newEntity (Position 1, Velocity 2, Flying)

  cmap $ \(Position p, Velocity v) -> Position (v+p)
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

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f = JSaddle.debugOr 8080 (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = do
  world <- initWorld
  putStrLn "start"
  runApp $ do 
    c <- askJSM
    let update = updateModel (c, world)
    startApp App { 
      update = update, ..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = 0
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: (JSContextRef, World) ->  Action -> Model -> Effect Action Model
updateModel (_, w) AddOne m = m <# do
  liftIO (runSystem game w)
  liftIO (putStrLn "Hello World") >> pure NoOp
updateModel (c, w) SubtractOne m = do
  m <# do
    liftIO (runSystem (drawEntity c) w)
    pure NoOp
  noEff (m - 1)
updateModel (_, w) NoOp m = noEff m
updateModel (_, w) SayHelloWorld m = m <# do
  liftIO (runSystem game w)
  liftIO (putStrLn "Hello World") >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (ms x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 , canvas_ [ id_ "canvas"
            , width_ "400"
            , height_ "300"
            ] []
 , audio_ [ src_ "assets/se/button.mp3", id_ "buttonSe" ] []
 ]
