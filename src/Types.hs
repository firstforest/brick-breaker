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
import JSDOM.Types (Comment, JSContextRef)
import Linear (V2 (..))
import Pixi (Application)

type Pos = V2 Float

newtype Position = Position Pos deriving (Show)

instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity Pos deriving (Show)

instance Component Velocity where type Storage Velocity = Map Velocity

data Collider = Collider
  { left :: Float,
    top :: Float,
    right :: Float,
    bottom :: Float
  }
  deriving (Show)

instance Component Collider where type Storage Collider = Map Collider

data Bar = Bar
  { length :: Float
  }
  deriving (Show)

instance Component Bar where type Storage Bar = Unique Bar

-- Ball

data Ball = Ball
  { id :: String,
    radius :: Float
  }
  deriving (Show)

instance Component Ball where type Storage Ball = Map Ball

data Block = Block
  { blockId :: String
  }
  deriving (Show)

instance Component Block where type Storage Block = Map Block

makeWorld "World" [''Position, ''Velocity, ''Bar, ''Ball, ''Collider, ''Block]

type System' a = System World a

data Context = Context
  { jsContext :: JSContextRef,
    pixiApp :: Application,
    world :: World
  }
