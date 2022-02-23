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
  return ()

game :: System' ()
game = do
  newEntity (Position 0, Velocity 1)
  newEntity (Position 2, Velocity 1)

  cmap $ \(Position p, Velocity v) -> Position (v + p)
