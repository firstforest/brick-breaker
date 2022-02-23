{-# LANGUAGE OverloadedStrings #-}

module Audio where

import Apecs
import JSDOM.Custom.AudioContext (newAudioContext)
import JSDOM.Generated.HTMLMediaElement (play)
import JSDOM.Types
import Miso

playSe c = do
  liftIO $
    flip runJSM c $ do
      audioContext <- newAudioContext
      jsval <- getElementById "buttonSe"
      audioElement <- fromJSValUnchecked jsval :: JSM HTMLAudioElement
      play audioElement