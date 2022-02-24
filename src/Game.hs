{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Game where

import Apecs
import qualified Const
import Control.Monad (forM_, when)
import Linear
import Types

ballSpeed :: Float
ballSpeed = 500

initialize :: System' ()
initialize = do
  newEntity
    ( Position $ V2 (Const.width / 2) (Const.height - 20),
      Bar 100,
      Collider 0 0 100 10
    )
  forM_ [0 .. 0] $ \i -> do
    newEntity
      ( Position $ V2 ((Const.width / 2) + i) (Const.height - 50),
        Ball ("ball" <> show i) 10,
        Velocity (ballSpeed *^ normalize (V2 10 (-3 - i)))
      )
  newEntity (Collider (-500) 0 0 Const.height) --left
  newEntity (Collider Const.width 0 (Const.width + 500) Const.height) --right
  newEntity (Collider 0 (-500) Const.width 0) -- top
  return ()

data CollisionResult
  = ToLeft
  | ToRight
  | ToUp
  | ToDown
  | AsIs

checkCollision left top right bottom x y radius
  | left < x - radius && x + radius < right =
    if top < y && y - radius < bottom
      then ToDown
      else
        if y < bottom && top < y + radius
          then ToUp
          else AsIs
  | top < y - radius && y + radius < bottom =
    if left < x && x - radius < right
      then ToRight
      else
        if x < right && left < x + radius
          then ToLeft
          else AsIs
  | otherwise = AsIs

step :: Float -> System' ()
step dt = do
  cmap $ \(Position p, Velocity v) -> Position ((dt *^ v) + p)
  cmapM_ $ \(Collider left top right bottom, _ :: Not Position) -> do
    let check = checkCollision left top right bottom
    cmap $ \(Ball _ radius, p@(Position (V2 x y)), v@(Velocity (V2 vx vy))) ->
      case check x y radius of
        ToLeft -> (Velocity (V2 (- (abs vx)) vy), Position (V2 (left - radius) y))
        ToRight -> (Velocity $ V2 (abs vx) vy, Position (V2 (right + radius) y))
        ToUp -> (Velocity $ V2 vx (- (abs vy)), Position (V2 x (top - radius)))
        ToDown -> (Velocity $ V2 vx (abs vy), Position (V2 x (bottom + radius)))
        AsIs -> (v, p)
  cmapM_ $ \(Collider left top right bottom, Position (V2 x y)) -> do
    let l = left - right / 2 + x
        t = top - bottom / 2 + y
        r = right - right / 2 + x
        b = bottom - bottom / 2 + y
        check = checkCollision l t r b
    cmap $ \(Ball _ radius, p@(Position (V2 x y)), v@(Velocity (V2 vx vy))) ->
      case check x y radius of
        ToLeft -> (Velocity (V2 (- (abs vx)) vy), Position (V2 (l - radius) y))
        ToRight -> (Velocity $ V2 (abs vx) vy, Position (V2 (r + radius) y))
        ToUp -> (Velocity $ V2 vx (- (abs vy)), Position (V2 x (t - radius)))
        ToDown -> (Velocity $ V2 vx (abs vy), Position (V2 x (b + radius)))
        AsIs -> (v, p)

moveBar :: Float -> System' ()
moveBar x = do
  cmap $ \(Bar _, Position (V2 _ y)) -> do
    Position $ V2 x y