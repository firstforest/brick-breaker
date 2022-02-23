{-# LANGUAGE OverloadedStrings #-}

module DevServer where

import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai as Wai
import           Network.WebSockets
import qualified Network.Wai.Application.Static as Static
import Network.Wai.Handler.Warp
import JSDOM (syncPoint)

fileServer :: Wai.Application
fileServer = Static.staticApp settings
  where
    settings = Static.defaultWebAppSettings "."

debugOr port f js b = do
  debugWrapper $ \withRefresh registerContext ->
    runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
      jsaddleOr defaultConnectionOptions (registerContext >> f >> syncPoint) (withRefresh $ jsaddleAppWithJsOr (jsaddleJs True <> js) b)
  putStrLn $ "<a href=\"http://localhost:" <> show port <> "\">run</a>"
