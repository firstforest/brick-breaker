{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Game where

import Apecs
import Types
import qualified Const
import Linear

initialize :: System' ()
initialize = do
  newEntity (Position $ V2 (Const.width/2) (Const.height - 20), Bar 100)
  newEntity (Position $ V2 (Const.width/2) (Const.height - 30), Ball 10, Velocity (V2 0 (-1)))
  return ()

step :: System' ()
step = do
  cmap $ \(Position p, Velocity v) -> Position (v + p)

moveBar :: Float -> System' ()
moveBar x = do
  cmap $ \(Bar _, Position (V2 _ y)) -> do
    Position $ V2 x y