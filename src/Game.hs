{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Game where

import Apecs
import qualified Const
import Control.Monad (when)
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
  newEntity
    ( Position $ V2 (Const.width / 2) (Const.height - 30),
      Ball 5,
      Velocity (ballSpeed *^ normalize (V2 10 (-3)))
    )
  newEntity (Collider (-50) 0 0 Const.height) --left
  newEntity (Collider Const.width 0 (Const.width + 50) Const.height) --right
  newEntity (Collider 0 (-50) Const.width 0) -- top
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
    cmap $ \(Ball radius, Position (V2 x y), v@(Velocity (V2 vx vy))) ->
      case check x y radius of
        ToLeft -> Velocity (V2 (- (abs vx)) vy)
        ToRight -> Velocity $ V2 (abs vx) vy
        ToUp -> Velocity $ V2 vx (- (abs vy))
        ToDown -> Velocity $ V2 vx (abs vy)
        AsIs -> v
  cmapM_ $ \(Collider left top right bottom, Position (V2 x y)) -> do
    let check = checkCollision (left - right / 2 + x) (top - bottom / 2 + y) (right - right / 2 + x) (bottom - bottom / 2 + y)
    cmap $ \(Ball radius, Position (V2 x y), v@(Velocity (V2 vx vy))) ->
      case check x y radius of
        ToLeft -> Velocity (V2 (- (abs vx)) vy)
        ToRight -> Velocity $ V2 (abs vx) vy
        ToUp -> Velocity $ V2 vx (- (abs vy))
        ToDown -> Velocity $ V2 vx (abs vy)
        AsIs -> v

moveBar :: Float -> System' ()
moveBar x = do
  cmap $ \(Bar _, Position (V2 _ y)) -> do
    Position $ V2 x y