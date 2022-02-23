{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Apecs
import JSDOM.Types (JSContextRef)
import Linear (V2 (..))
import Pixi (Application)

type Pos = V2 Float

newtype Position = Position Pos deriving (Show)

instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity Pos deriving (Show)

instance Component Velocity where type Storage Velocity = Map Velocity

data Bar = Bar
  { length :: Float
  }
  deriving (Show)

instance Component Bar where type Storage Bar = Unique Bar

makeWorld "World" [''Position, ''Velocity, ''Bar]

type System' a = System World a

data Context = Context
  { jsContext :: JSContextRef,
    pixiApp :: Application,
    world :: World
  }
